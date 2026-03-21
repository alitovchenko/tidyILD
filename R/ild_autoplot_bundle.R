# Section-first autoplot for ild_diagnostics_bundle (see ?ild_autoplot)

#' @rdname ild_autoplot
#' @method ild_autoplot ild_diagnostics_bundle
#' @export
ild_autoplot.ild_diagnostics_bundle <- function(x, section = "residual", type = NULL, ...) {
  if (!inherits(x, "ild_diagnostics_bundle")) {
    stop("Expected an ild_diagnostics_bundle.", call. = FALSE)
  }
  section <- match.arg(section, c("residual", "fit", "predictive", "data", "design", "causal"))

  if (identical(section, "residual")) {
    leg <- x$residual$legacy_ild_diagnostics
    if (is.null(type)) {
      if (!is.null(leg)) {
        return(plot_ild_diagnostics(leg, ...))
      }
      type <- "acf"
    }
    type <- match.arg(type, c("acf", "qq", "fitted"))
    switch(type,
      acf = plot_bundle_residual_acf(x),
      qq = plot_bundle_residual_qq(x),
      fitted = plot_bundle_residual_fitted(x)
    )
  } else {
    choices <- switch(section,
      fit = c("convergence"),
      predictive = c("ppc"),
      data = c("missingness"),
      design = c("coverage"),
      causal = c("weights")
    )
    if (is.null(type)) {
      type <- choices[1L]
    }
    type <- match.arg(type, choices)
    switch(section,
      fit = plot_bundle_fit_convergence(x),
      predictive = plot_bundle_predictive_ppc(x, ...),
      data = plot_bundle_data_missingness(x),
      design = plot_bundle_design_coverage(x),
      causal = plot_bundle_causal_weights(x)
    )
  }
}

#' @keywords internal
#' @noRd
plot_bundle_residual_acf <- function(x) {
  leg <- x$residual$legacy_ild_diagnostics
  if (!is.null(leg)) {
    pl <- plot_ild_diagnostics(leg, type = "residual_acf")
    return(pl$residual_acf)
  }
  fit <- attr(x, "ild_fit", exact = TRUE)
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(fit) || is.null(dat)) {
    stop(
      "Residual ACF requires residual$legacy_ild_diagnostics or a bundle from ",
      "ild_diagnose() (with ild_fit / ild_data attributes). Re-run ild_diagnose().",
      call. = FALSE
    )
  }
  diag <- ild_diagnostics(fit, data = dat, type = "residual_acf", by_id = TRUE)
  plot_ild_diagnostics(diag, type = "residual_acf")$residual_acf
}

#' @keywords internal
#' @noRd
plot_bundle_residual_qq <- function(x) {
  leg <- x$residual$legacy_ild_diagnostics
  if (!is.null(leg)) {
    pl <- plot_ild_diagnostics(leg, type = "qq")
    return(pl$qq)
  }
  fit <- attr(x, "ild_fit", exact = TRUE)
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(fit) || is.null(dat)) {
    stop(
      "Residual Q-Q requires residual$legacy_ild_diagnostics or ild_diagnose() ",
      "with ild_fit / ild_data attributes.",
      call. = FALSE
    )
  }
  diag <- ild_diagnostics(fit, data = dat, type = "qq", by_id = TRUE)
  plot_ild_diagnostics(diag, type = "qq")$qq
}

#' @keywords internal
#' @noRd
plot_bundle_residual_fitted <- function(x) {
  fit <- attr(x, "ild_fit", exact = TRUE)
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(fit) || is.null(dat)) {
    stop(
      "Fitted vs observed requires attr(bundle, \"ild_fit\") and attr(bundle, \"ild_data\"). ",
      "Re-run ild_diagnose() on the model.",
      call. = FALSE
    )
  }
  ild_plot_fitted(fit, dat)
}

#' @keywords internal
#' @noRd
plot_bundle_fit_convergence <- function(x) {
  fd <- x$fit
  eng <- if (!is.null(fd$engine)) fd$engine else x$meta$engine

  if (identical(eng, "brms")) {
    ct <- fd$convergence_table
    if (is.null(ct) || nrow(ct) == 0L) {
      return(
        ggplot2::ggplot() +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "Convergence (no fixed-effects summary table)")
      )
    }
    ggplot2::ggplot(ct, ggplot2::aes(
      x = stats::reorder(.data$term, .data$rhat),
      y = .data$rhat
    )) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 1.05, linetype = 2, alpha = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "R-hat", title = "Convergence (fixed effects)") +
      ggplot2::theme_minimal()
  }

  txt <- paste(
    c(
      sprintf("Engine: %s", if (!is.null(fd$engine)) fd$engine else eng),
      sprintf("Singular: %s", fd$singular),
      sprintf("Converged: %s", fd$converged),
      if (length(fd$optimizer_messages) > 0L) {
        paste("Optimizer:", paste(fd$optimizer_messages, collapse = "; "))
      } else {
        "Optimizer: (none)"
      }
    ),
    collapse = "\n"
  )
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = txt, hjust = 0.5, vjust = 0.5) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Convergence (frequentist)") +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}

#' @keywords internal
#' @noRd
plot_bundle_predictive_ppc <- function(x, ...) {
  eng <- x$meta$engine
  if (!identical(eng, "brms")) {
    stop(
      "Posterior predictive checks (section = \"predictive\", type = \"ppc\") ",
      "require a brmsfit from ild_brms().",
      call. = FALSE
    )
  }
  fit <- attr(x, "ild_fit", exact = TRUE)
  if (is.null(fit)) {
    stop("PPC requires attr(bundle, \"ild_fit\"). Re-run ild_diagnose() on the brmsfit.", call. = FALSE)
  }
  rlang::check_installed("brms")
  brms::pp_check(fit, ...)
}

#' @keywords internal
#' @noRd
plot_bundle_data_missingness <- function(x) {
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(dat)) {
    stop(
      "Missingness plot requires attr(bundle, \"ild_data\"). Re-run ild_diagnose().",
      call. = FALSE
    )
  }
  mp <- ild_missing_pattern(dat, vars = NULL)
  mp$plot
}

#' @keywords internal
#' @noRd
plot_bundle_design_coverage <- function(x) {
  dc <- x$design$ild_design_check
  dat <- attr(x, "ild_data", exact = TRUE)

  if (!is.null(dc$wp_bp) && nrow(dc$wp_bp) > 0L) {
    wp <- dc$wp_bp
    tot <- wp$wp_var + wp$bp_var
    pct_wp <- ifelse(tot > 0, 100 * wp$wp_var / tot, NA_real_)
    pct_bp <- ifelse(tot > 0, 100 * wp$bp_var / tot, NA_real_)
    n <- nrow(wp)
    df <- data.frame(
      variable = rep(as.character(wp$variable), 2L),
      component = rep(c("within-person", "between-person"), each = n),
      pct = c(pct_wp, pct_bp),
      stringsAsFactors = FALSE
    )
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$variable, y = .data$pct, fill = .data$component)) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::labs(
          x = NULL,
          y = "Percent of total variance",
          title = "Design coverage (within vs between)",
          fill = NULL
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    )
  }

  if (!is.null(dat) && ".ild_dt" %in% names(dat)) {
    dt <- dat[[".ild_dt"]]
    dt <- dt[!is.na(dt)]
    if (length(dt) == 0L) {
      stop("No non-missing .ild_dt values for interval coverage plot.", call. = FALSE)
    }
    df <- data.frame(dt = dt)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$dt)) +
        ggplot2::geom_histogram(bins = min(30L, max(10L, length(unique(dt)))), fill = "steelblue", color = "white") +
        ggplot2::labs(
          x = expression(Delta * "t (observation intervals)"),
          y = "Count",
          title = "Observation interval coverage",
          subtitle = "WP/BP decomposition unavailable (no model vars in design check)"
        ) +
        ggplot2::theme_minimal()
    )
  }

  stop(
    "Design coverage needs design$ild_design_check$wp_bp or ILD data with `.ild_dt`.",
    call. = FALSE
  )
}

#' @keywords internal
#' @noRd
plot_bundle_causal_weights <- function(x) {
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(dat)) {
    stop("Weights plot requires attr(bundle, \"ild_data\"). Re-run ild_diagnose().", call. = FALSE)
  }
  if (!(".ipw" %in% names(dat))) {
    stop(
      "No `.ipw` column in ild_data. Use ild_ipw_weights() or add IPW columns first.",
      call. = FALSE
    )
  }
  w <- dat[[".ipw"]]
  w <- w[is.finite(w)]
  if (length(w) == 0L) {
    stop("No finite .ipw values to plot.", call. = FALSE)
  }
  df <- data.frame(w = w)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$w)) +
    ggplot2::geom_histogram(bins = min(30L, max(10L, length(unique(w)))), fill = "gray40", color = "white") +
    ggplot2::labs(x = "Weight", y = "Count", title = "Causal / IPW weights (.ipw)") +
    ggplot2::theme_minimal()
}
