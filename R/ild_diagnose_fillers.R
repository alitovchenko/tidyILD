# Internal fillers for ild_diagnostics_bundle assembly (engine-agnostic contract).
# Standalone section providers are documented under ?ild_diagnostics_utilities
# (ild_design_check, ild_missing_pattern, ild_missing_model, ild_ipw_weights).

#' Data-layer diagnostics (ILD object)
#' @param outcome_vars Optional character vector of column names for \code{outcome_summaries} and \code{missingness_rates}.
#' @keywords internal
#' @noRd
fill_diagnostics_data <- function(data, include_missing_pattern = TRUE, outcome_vars = NULL) {
  validate_ild(data)
  sm <- ild_summary(data)
  sc <- ild_spacing_class(data)
  mp <- NULL
  if (isTRUE(include_missing_pattern)) {
    mp <- tryCatch(ild_missing_pattern(data, vars = NULL), error = function(e) NULL)
  }
  n_int <- if (!is.null(sm$spacing$n_intervals)) sm$spacing$n_intervals else {
    sum(!is.na(data[[".ild_dt"]]))
  }
  cohort <- list(
    n_id = as.integer(sm$summary$n_id[1]),
    n_obs = as.integer(sm$summary$n_obs[1]),
    n_intervals = as.integer(n_int)
  )
  tab <- as.integer(table(data[[".ild_id"]]))
  obs_per_id <- list(
    min = as.integer(min(tab)),
    median = stats::median(tab),
    max = as.integer(max(tab)),
    sd = if (length(tab) > 1L) stats::sd(as.numeric(tab)) else NA_real_
  )
  outcome_summaries <- NULL
  missingness_rates <- NULL
  if (!is.null(outcome_vars) && length(outcome_vars) > 0L) {
    outcome_vars <- unique(outcome_vars[outcome_vars %in% names(data)])
    if (length(outcome_vars) > 0L) {
      rows <- lapply(outcome_vars, function(v) {
        x <- data[[v]]
        ntot <- length(x)
        nna <- sum(is.na(x))
        tibble::tibble(
          variable = v,
          mean = if (is.numeric(x)) mean(x, na.rm = TRUE) else NA_real_,
          sd = if (is.numeric(x)) stats::sd(x, na.rm = TRUE) else NA_real_,
          min = if (is.numeric(x)) suppressWarnings(min(x, na.rm = TRUE)) else NA_real_,
          max = if (is.numeric(x)) suppressWarnings(max(x, na.rm = TRUE)) else NA_real_,
          pct_na = if (ntot > 0L) 100 * nna / ntot else NA_real_,
          n = ntot
        )
      })
      outcome_summaries <- do.call(rbind, rows)
      missingness_rates <- tibble::tibble(
        variable = outcome_vars,
        pct_na = vapply(outcome_vars, function(v) {
          x <- data[[v]]
          if (length(x) == 0L) return(NA_real_)
          100 * mean(is.na(x))
        }, double(1L))
      )
    }
  }
  list(
    summary = sm$summary,
    spacing_class = sc,
    n_gaps = sm$n_gaps,
    pct_gap = sm$pct_gap,
    median_dt_sec = sm$summary$median_dt_sec[1],
    iqr_dt_sec = sm$summary$iqr_dt_sec[1],
    time_range = sm$time_range,
    cohort = cohort,
    obs_per_id = obs_per_id,
    outcome_summaries = outcome_summaries,
    missingness_rates = missingness_rates,
    missing_pattern = if (!is.null(mp)) {
      list(summary = mp$summary, overall = mp$overall, n_complete = mp$n_complete)
    } else {
      NULL
    }
  )
}

#' Design-layer diagnostics
#' @keywords internal
#' @noRd
fill_diagnostics_design <- function(data, vars = NULL) {
  validate_ild(data)
  dc <- ild_design_check(data, vars = vars)
  sm <- ild_summary(data)
  tr <- sm$time_range
  span_sec <- if (length(tr) >= 2L && all(is.finite(tr))) (tr[2L] - tr[1L]) else NA_real_
  time_coverage <- list(
    min = if (length(tr) >= 1L) tr[1L] else NA_real_,
    max = if (length(tr) >= 2L) tr[2L] else NA_real_,
    span_sec = span_sec
  )
  has_wp_bp <- !is.null(dc$wp_bp) && (inherits(dc$wp_bp, "data.frame") || inherits(dc$wp_bp, "tbl_df")) &&
    nrow(dc$wp_bp) > 0L
  flags <- list(
    has_wp_bp = has_wp_bp,
    spacing_class = dc$spacing_class,
    irregular = identical(dc$spacing_class, "irregular-ish")
  )
  list(
    ild_design_check = dc,
    spacing_class = dc$spacing_class,
    recommendation = dc$recommendation,
    wp_bp = dc$wp_bp,
    design_missingness = dc$missingness,
    flags = flags,
    time_coverage = time_coverage
  )
}

#' Fit diagnostics — lmerMod
#' @keywords internal
#' @noRd
fill_diagnostics_fit_lmerMod <- function(fit) {
  out <- list(
    engine = "lmer",
    singular = NA,
    converged = NA,
    optimizer_messages = character(),
    optinfo = NULL,
    reml = NA,
    optimizer = NA_character_,
    theta_summary = NULL,
    X_rank = NA_integer_,
    residual_correlation = list(
      modeled = FALSE,
      structure = "none",
      note = "lme4::lmer has no Gaussian AR1/CAR1 residual correlation; use nlme::lme via ild_lme(ar1 = TRUE)."
    )
  )
  if (!inherits(fit, "lmerMod")) return(out)
  out$singular <- lme4::isSingular(fit)
  out$reml <- tryCatch(lme4::getME(fit, "is_REML"), error = function(e) NA)
  out$optimizer <- tryCatch(as.character(fit@optinfo$optimizer)[1L], error = function(e) NA_character_)
  th <- tryCatch(lme4::getME(fit, "theta"), error = function(e) NULL)
  if (!is.null(th) && length(th) > 0L) {
    out$theta_summary <- list(
      length = length(th),
      min = min(th, na.rm = TRUE),
      max = max(th, na.rm = TRUE)
    )
  }
  X <- tryCatch(lme4::getME(fit, "X"), error = function(e) NULL)
  if (!is.null(X) && nrow(X) > 0L) {
    out$X_rank <- tryCatch(qr(X)$rank, error = function(e) NA_integer_)
  }
  ar1 <- isTRUE(attr(fit, "ild_ar1", exact = TRUE))
  out$residual_correlation <- list(
    modeled = ar1,
    structure = if (ar1) "ild_ar1_attr" else "none",
    note = out$residual_correlation$note
  )
  opt <- fit@optinfo
  out$optinfo <- list(
    conv = opt$conv,
    finopt = opt$finopt
  )
  cv <- opt$conv$lme4
  if (!is.null(cv)) {
    if (is.list(cv)) {
      nums <- suppressWarnings(as.numeric(unlist(cv, use.names = FALSE)))
      out$converged <- length(nums) > 0L && all(nums == 0, na.rm = TRUE)
    } else {
      out$converged <- isTRUE(as.integer(cv)[1L] == 0L)
    }
  }
  if (length(opt$warnings) > 0L) {
    out$optimizer_messages <- vapply(
      opt$warnings,
      function(w) tryCatch(conditionMessage(w), error = function(e) "warning"),
      character(1L)
    )
  }
  out
}

#' Fit diagnostics — nlme lme
#' @keywords internal
#' @noRd
fill_diagnostics_fit_lme <- function(fit) {
  out <- list(
    engine = "lme",
    singular = NA,
    converged = NA,
    optimizer_messages = character(),
    apVar_ok = NA,
    residual_correlation = list(modeled = FALSE, class = NA_character_, coef_corStruct = NULL)
  )
  if (!inherits(fit, "lme")) return(out)
  out$converged <- TRUE
  out$apVar_ok <- !is.null(fit$apVar) && !any(is.na(fit$apVar))
  if (!is.null(fit$apVar) && any(is.na(fit$apVar))) {
    out$singular <- TRUE
  } else {
    out$singular <- FALSE
  }
  corr_class <- attr(fit, "ild_correlation_class", exact = TRUE)
  coef_cs <- tryCatch(
    if (!is.null(fit$modelStruct$corStruct)) coef(fit$modelStruct$corStruct) else NULL,
    error = function(e) NULL
  )
  has_cor <- !is.null(fit$modelStruct$corStruct)
  out$residual_correlation <- list(
    modeled = isTRUE(attr(fit, "ild_ar1", exact = TRUE)) || has_cor,
    class = if (!is.null(corr_class)) as.character(corr_class)[1L] else NA_character_,
    coef_corStruct = coef_cs
  )
  out
}

#' Fit diagnostics — brms (uses attr ild_posterior)
#' @keywords internal
#' @noRd
fill_diagnostics_fit_brms <- function(fit) {
  ps <- attr(fit, "ild_posterior", exact = TRUE)
  if (is.null(ps)) ps <- ild_posterior_attr(fit, prior_template = NULL)
  conv_tbl <- NULL
  sf <- tryCatch(summary(fit)$fixed, error = function(e) NULL)
  if (!is.null(sf) && nrow(sf) > 0L) {
    conv_tbl <- tibble::tibble(
      term = rownames(sf),
      rhat = as.numeric(sf[["Rhat"]]),
      ess_bulk = as.numeric(sf[["Bulk_ESS"]]),
      ess_tail = as.numeric(sf[["Tail_ESS"]])
    )
  }
  list(
    engine = "brms",
    ild_posterior = ps,
    convergence_table = conv_tbl,
    max_rhat = ild_brms_max_rhat(fit),
    n_divergent = ps$n_divergent,
    n_max_treedepth_hits = ps$n_max_treedepth_hits,
    residual_correlation = list(
      modeled = NA,
      note = "See brms family/residual structure and posterior predictive checks for residual behavior."
    )
  )
}

#' Residual diagnostics — frequentist (embeds legacy [ild_diagnostics()] for plotting)
#' @keywords internal
#' @noRd
fill_diagnostics_residual_frequentist <- function(object, data, type, by_id) {
  diag <- ild_diagnostics(object, data = data, type = type, by_id = by_id)
  aug <- tryCatch(augment_ild_model(object), error = function(e) NULL)
  res_sd <- NA_real_
  cor_obs_fitted <- NA_real_
  if (!is.null(aug) && nrow(aug) > 0L) {
    res_sd <- stats::sd(aug$.resid, na.rm = TRUE)
    if (all(is.finite(aug$.fitted)) && stats::sd(aug$.fitted, na.rm = TRUE) > 0) {
      cor_obs_fitted <- stats::cor(aug$.outcome, aug$.fitted, use = "pairwise.complete.obs")
    }
  }
  list(
    engine = if (inherits(object, "lme")) "lme" else "lmer",
    legacy_ild_diagnostics = diag,
    residual_sd = res_sd,
    cor_observed_fitted = cor_obs_fitted,
    stats = diag$stats,
    meta = diag$meta
  )
}

#' Residual layer — brms (lighter; legacy ACF not computed here)
#' @keywords internal
#' @noRd
fill_diagnostics_residual_brms <- function(fit) {
  aug <- tryCatch(ild_augment(fit), error = function(e) NULL)
  out <- list(engine = "brms", legacy_ild_diagnostics = NULL, residual_sd = NA_real_)
  if (!is.null(aug) && ".resid" %in% names(aug)) {
    out$residual_sd <- stats::sd(aug$.resid, na.rm = TRUE)
    out$mean_abs_resid <- mean(abs(aug$.resid), na.rm = TRUE)
  }
  out
}

#' Predictive layer — frequentist (observed vs fitted summaries)
#' @keywords internal
#' @noRd
fill_diagnostics_predictive_frequentist <- function(object, data) {
  aug <- tryCatch(augment_ild_model(object), error = function(e) NULL)
  if (is.null(aug)) return(NULL)
  c(
    list(engine = if (inherits(object, "lme")) "lme" else "lmer"),
    fill_diagnostics_predictive_obs_metrics(aug)
  )
}

#' @keywords internal
#' @noRd
fill_diagnostics_predictive_obs_metrics <- function(aug) {
  if (is.null(aug) || !all(c(".outcome", ".fitted") %in% names(aug))) {
    return(list())
  }
  y <- aug$.outcome
  f <- aug$.fitted
  ok <- is.finite(y) & is.finite(f)
  n <- sum(ok)
  ye <- y[ok]
  fe <- f[ok]
  res <- ye - fe
  list(
    n = as.integer(n),
    mean_abs_error = if (n > 0L) mean(abs(res), na.rm = TRUE) else NA_real_,
    rmse = if (n > 0L) sqrt(mean(res^2, na.rm = TRUE)) else NA_real_,
    mean_residual = if (n > 0L) mean(res, na.rm = TRUE) else NA_real_,
    max_abs_error = if (n > 0L) max(abs(res), na.rm = TRUE) else NA_real_,
    cor_observed_fitted = if (n > 1L) stats::cor(ye, fe, use = "pairwise.complete.obs") else NA_real_
  )
}

#' Predictive layer — brms augment-only (no PPC)
#' @keywords internal
#' @noRd
fill_diagnostics_predictive_brms_augment_only <- function(fit) {
  aug <- tryCatch(ild_augment(fit), error = function(e) NULL)
  c(list(engine = "brms"), fill_diagnostics_predictive_obs_metrics(aug))
}

#' Predictive layer — brms (PPC summary)
#' @keywords internal
#' @noRd
fill_diagnostics_predictive_brms <- function(fit, ppc_ndraws) {
  ppc <- ild_brms_ppc_summary(fit, ndraws = ppc_ndraws)
  aug <- tryCatch(ild_augment(fit), error = function(e) NULL)
  c(
    list(engine = "brms", ppc = ppc),
    fill_diagnostics_predictive_obs_metrics(aug)
  )
}

#' Missingness-focused slice (formula / model variables when available)
#' @keywords internal
#' @noRd
fill_diagnostics_missingness_section <- function(data, vars = NULL) {
  validate_ild(data)
  if (is.null(vars) || length(vars) == 0L) {
    return(list(
      note = "No model variables supplied; see data$missing_pattern for global missingness.",
      summary = NULL,
      overall = NULL,
      pct_na_by_var = NULL
    ))
  }
  mp <- tryCatch(ild_missing_pattern(data, vars = vars), error = function(e) NULL)
  if (is.null(mp)) {
    return(list(
      note = NULL,
      summary = NULL,
      overall = NULL,
      pct_na_by_var = NULL
    ))
  }
  pct_na_by_var <- NULL
  if (!is.null(mp$summary) && nrow(mp$summary) > 0L && "pct_na" %in% names(mp$summary)) {
    pct_na_by_var <- mp$summary[, intersect(c("variable", "pct_na"), names(mp$summary)), drop = FALSE]
  }
  list(
    note = NULL,
    summary = mp$summary,
    overall = mp$overall,
    pct_na_by_var = pct_na_by_var
  )
}

#' Causal / IPW slice when weight columns present
#' @keywords internal
#' @noRd
fill_diagnostics_causal <- function(data, causal_detail = FALSE) {
  if (is.null(data) || !is_ild(data)) return(NULL)
  wcols <- grep("^\\.ipw|ipw_weight|\\.w_", names(data), value = TRUE, ignore.case = TRUE)
  if (length(wcols) == 0L) return(NULL)
  out <- list(columns_found = wcols)
  if (".ipw" %in% names(data)) {
    w <- data[[".ipw"]]
    out$weight_summary <- list(
      min = min(w, na.rm = TRUE),
      max = max(w, na.rm = TRUE),
      mean = mean(w, na.rm = TRUE)
    )
    if (isTRUE(causal_detail)) {
      wf <- w[is.finite(w)]
      if (length(wf) > 0L) {
        qs <- stats::quantile(wf, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE, names = TRUE)
        out$weight_detail <- list(
          quantiles = qs,
          sum_w = sum(wf, na.rm = TRUE)
        )
      }
    }
  }
  out
}

#' Build warnings tibble
#' @param rows List of named lists with source, level, message, code
#' @keywords internal
#' @noRd
collect_diagnostics_warnings <- function(rows = list()) {
  if (length(rows) == 0L) {
    return(tibble::tibble(
      source = character(),
      level = character(),
      message = character(),
      code = character()
    ))
  }
  tibble::tibble(
    source = vapply(rows, function(z) .fld(z, "source"), character(1)),
    level = vapply(rows, function(z) .fld(z, "level"), character(1)),
    message = vapply(rows, function(z) .fld(z, "message"), character(1)),
    code = vapply(rows, function(z) .fld(z, "code"), character(1))
  )
}

#' @keywords internal
#' @noRd
.fld <- function(z, nm) {
  v <- z[[nm]]
  if (is.null(v)) NA_character_ else as.character(v)[1]
}

#' Build guardrails tibble (delegates to registry; uses \code{rule_id} schema)
#' @keywords internal
#' @noRd
collect_diagnostics_guardrails <- function(rows = list()) {
  guardrail_finalize_rows(rows)
}

.fld2 <- function(z, nm) {
  v <- z[[nm]]
  if (is.null(v)) NA_character_ else paste(as.character(v), collapse = " ")
}

#' Assemble warnings for frequentist fits (guardrails via \code{\link{evaluate_guardrails_fit}})
#' @keywords internal
#' @noRd
collect_freq_warnings_guardrails <- function(fit, fit_diag) {
  warns <- list()
  if (inherits(fit, "lmerMod") && length(fit@optinfo$warnings) > 0L) {
    for (i in seq_along(fit@optinfo$warnings)) {
      warns[[length(warns) + 1L]] <- list(
        source = "lme4",
        level = "warning",
        message = conditionMessage(fit@optinfo$warnings[[i]]),
        code = "lmer_warning"
      )
    }
  }
  list(
    warnings = collect_diagnostics_warnings(warns),
    guardrails = guardrails_empty_tibble()
  )
}

#' @keywords internal
#' @noRd
collect_brms_warnings_guardrails <- function(fit, fit_diag) {
  warns <- list()
  ps <- fit_diag$ild_posterior
  if (!is.null(ps) && !is.na(ps$n_divergent) && ps$n_divergent > 0L) {
    warns[[length(warns) + 1L]] <- list(
      source = "stan",
      level = "warning",
      message = sprintf("%s divergent transition(s) after warmup.", ps$n_divergent),
      code = "divergent_transitions"
    )
  }
  list(
    warnings = collect_diagnostics_warnings(warns),
    guardrails = guardrails_empty_tibble()
  )
}

#' Collapse bundle into short narrative strings
#' @keywords internal
#' @noRd
build_diagnostics_bundle_summary <- function(bundle) {
  parts <- character()
  if (!is.null(bundle$meta$engine)) {
    parts <- c(parts, sprintf("Engine: %s.", bundle$meta$engine))
  }
  if (!is.null(bundle$fit)) {
    if (!is.null(bundle$fit$singular) && !is.na(bundle$fit$singular) && bundle$fit$singular) {
      parts <- c(parts, "Singular / problematic fit variance structure reported.")
    }
    if (!is.null(bundle$fit$X_rank) && !is.na(bundle$fit$X_rank)) {
      parts <- c(parts, sprintf("Fixed-effects design rank (X): %s.", bundle$fit$X_rank))
    }
    if (!is.null(bundle$fit$max_rhat) && is.finite(bundle$fit$max_rhat)) {
      parts <- c(parts, sprintf("Max R-hat: %.3f.", bundle$fit$max_rhat))
    }
  }
  if (!is.null(bundle$data$obs_per_id)) {
    opi <- bundle$data$obs_per_id
    if (!is.null(opi$min) && !is.null(opi$max)) {
      parts <- c(parts, sprintf(
        "Observations per person: min = %s, median = %.1f, max = %s.",
        opi$min, opi$median, opi$max
      ))
    }
  }
  if (!is.null(bundle$data$outcome_summaries) && nrow(bundle$data$outcome_summaries) > 0L &&
        "pct_na" %in% names(bundle$data$outcome_summaries)) {
    pm <- suppressWarnings(max(bundle$data$outcome_summaries$pct_na, na.rm = TRUE))
    if (is.finite(pm) && pm > 0) {
      parts <- c(parts, sprintf("Outcome(s) missingness up to %.1f%% (see data$outcome_summaries).", pm))
    }
  }
  if (!is.null(bundle$residual$legacy_ild_diagnostics)) {
    d <- bundle$residual$legacy_ild_diagnostics
    parts <- c(parts, sprintf(
      "Residual diagnostics (%s): n_obs = %s.",
      d$meta$engine, d$meta$n_obs
    ))
  }
  if (!is.null(bundle$predictive) && is.list(bundle$predictive$ppc) &&
        is.null(bundle$predictive$ppc$error)) {
    p <- bundle$predictive$ppc
    parts <- c(parts, sprintf(
      "PPC: |mean_obs - mean_rep| = %.4f; |sd_obs - sd_rep| = %.4f.",
      p$mean_abs_diff, p$sd_abs_diff
    ))
  }
  if (nrow(bundle$warnings) > 0L) {
    parts <- c(parts, sprintf("%d diagnostic warning(s) recorded.", nrow(bundle$warnings)))
  }
  if (nrow(bundle$guardrails) > 0L) {
    parts <- c(parts, sprintf("%d guardrail(s) triggered (see guardrail_registry).", nrow(bundle$guardrails)))
  }
  parts
}
