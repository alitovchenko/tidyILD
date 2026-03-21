# Unified S3 API for fitting, tidying, augmentation, diagnostics, and plotting
# Backends: lme4 and nlme via ild_lme(); future engines can add methods.

#' Fit a mixed model to ILD data (unified entry point)
#'
#' Thin wrapper around [ild_lme()] that selects the estimation backend explicitly.
#' \code{backend = "lme4"} fits with [lme4::lmer()] (no residual autocorrelation).
#' \code{backend = "nlme"} fits with [nlme::lme()] and AR1/CAR1 residual structure.
#' Provenance records \code{backend} and package versions like direct [ild_lme()] calls.
#'
#' @param formula Passed to [ild_lme()].
#' @param data An ILD object (see [is_ild()]).
#' @param backend Character. \code{"lme4"} (default) or \code{"nlme"}.
#' @param correlation_class,random,warn_no_ar1,warn_uncentered Passed to [ild_lme()] when \code{backend = "nlme"}.
#' @param ... Passed to [ild_lme()] / [lme4::lmer()] or [nlme::lme()].
#' @return A fitted model with \code{ild_data} and \code{ild_provenance} attributes (see [ild_lme()]).
#' @export
ild_fit <- function(formula,
                    data,
                    backend = c("lme4", "nlme"),
                    correlation_class = c("auto", "AR1", "CAR1"),
                    random = ~ 1 | .ild_id,
                    warn_no_ar1 = TRUE,
                    warn_uncentered = TRUE,
                    ...) {
  validate_ild(data)
  backend <- match.arg(backend)
  if (backend == "lme4") {
    return(ild_lme(
      formula, data,
      ar1 = FALSE,
      warn_no_ar1 = warn_no_ar1,
      warn_uncentered = warn_uncentered,
      ...
    ))
  }
  ild_lme(
    formula, data,
    ar1 = TRUE,
    correlation_class = correlation_class,
    random = random,
    warn_no_ar1 = warn_no_ar1,
    warn_uncentered = warn_uncentered,
    ...
  )
}

#' Tidy fixed effects from an ILD model fit (S3 generic)
#'
#' Dispatches to [tidy_ild_model()] for \code{lmerMod} and \code{lme} objects from [ild_lme()],
#' or the \code{brmsfit} method for [ild_brms()]. All methods return tables conforming to
#' \code{\link{ild_tidy_schema}}.
#'
#' @param x A fitted model from [ild_lme()].
#' @param ... Passed to [tidy_ild_model()].
#' @export
ild_tidy <- function(x, ...) {
  UseMethod("ild_tidy")
}

#' @rdname ild_tidy
#' @method ild_tidy lmerMod
#' @export
ild_tidy.lmerMod <- function(x, ...) {
  tidy_ild_model(x, ...)
}

#' @rdname ild_tidy
#' @method ild_tidy lme
#' @export
ild_tidy.lme <- function(x, ...) {
  tidy_ild_model(x, ...)
}

#' @rdname ild_tidy
#' @method ild_tidy default
#' @export
ild_tidy.default <- function(x, ...) {
  stop(
    "No ild_tidy method for class ", paste(class(x), collapse = ", "),
    ". Use tidy_ild_model() for ild_lme() or ild_tidy() for ild_brms() fits.",
    call. = FALSE
  )
}

#' Augment observations with fitted values and residuals (S3 generic)
#'
#' Dispatches to [augment_ild_model()] for \code{lmerMod} and \code{lme} objects from [ild_lme()],
#' or the \code{brmsfit} method for [ild_brms()]. All methods return tables conforming to
#' \code{\link{ild_augment_schema}}.
#'
#' @param x A fitted model from [ild_lme()].
#' @param ... Passed to [augment_ild_model()].
#' @export
ild_augment <- function(x, ...) {
  UseMethod("ild_augment")
}

#' @rdname ild_augment
#' @method ild_augment lmerMod
#' @export
ild_augment.lmerMod <- function(x, ...) {
  augment_ild_model(x, ...)
}

#' @rdname ild_augment
#' @method ild_augment lme
#' @export
ild_augment.lme <- function(x, ...) {
  augment_ild_model(x, ...)
}

#' @rdname ild_augment
#' @method ild_augment default
#' @export
ild_augment.default <- function(x, ...) {
  stop(
    "No ild_augment method for class ", paste(class(x), collapse = ", "),
    ". Use augment_ild_model() for ild_lme() or ild_augment() for ild_brms() fits.",
    call. = FALSE
  )
}

#' Engine-agnostic diagnostics façade
#'
#' Returns a single \code{\link{ild_diagnostics_bundle}} for \code{lmerMod}, \code{lme},
#' and \code{brmsfit} (from [ild_lme()] / [ild_brms()]). Sections are parallel across engines;
#' see \code{?ild_diagnostics_bundle}. Residual ACF/Q-Q for frequentist models are stored in
#' \code{residual$legacy_ild_diagnostics} for [plot_ild_diagnostics()]. For raw residual-only
#' computation without the bundle, call [ild_diagnostics()] directly.
#'
#' **Building blocks:** the same analyses are available as standalone functions
#' ([ild_design_check()], [ild_missing_pattern()], etc.); see [ild_diagnostics_utilities].
#'
#' @param object A fitted model.
#' @param data Optional ILD data (defaults to \code{attr(object, "ild_data")}).
#' @param type Residual diagnostic types for \code{lmerMod}/\code{lme} (passed to [ild_diagnostics()]).
#'   For \code{brmsfit}: \code{"all"} or any of \code{"convergence"}, \code{"sampler"}, \code{"ppc"}.
#' @param by_id For \code{lmerMod}/\code{lme}: ACF by person.
#' @param ppc_ndraws For \code{brmsfit}: posterior predictive draws.
#' @param missing_model If \code{TRUE}, fit \code{\link{ild_missing_model}} for the response (optional; can be slow). Requires at least one predictor.
#' @param missing_model_predictors Character vector for \code{ild_missing_model}; default \code{NULL} uses formula predictors.
#' @param causal_detail If \code{TRUE}, add extra quantile summaries for \code{.ipw} when present.
#' @param ... Reserved.
#' @return An \code{\link{ild_diagnostics_bundle}}.
#' @seealso \code{\link{ild_autoplot}}, \code{\link{ild_diagnostics_bundle}},
#'   \code{\link{guardrail_registry}}, \code{\link{ild_diagnostics_utilities}},
#'   \code{\link{ild_diagnostics}}
#' @export
ild_diagnose <- function(object, ...) {
  UseMethod("ild_diagnose")
}

#' Autoplot for ILD model fits or diagnostics (S3 generic)
#'
#' For \code{ild_diagnostics_bundle} from [ild_diagnose()], dispatches by \code{section}
#' and \code{type} (section-first; see \code{?ild_autoplot} method for
#' \code{ild_diagnostics_bundle}). For [ild_diagnostics()], calls [plot_ild_diagnostics()]
#' directly. For \code{lmerMod} or \code{lme} fits from [ild_lme()], calls [ild_plot()]
#' (default \code{type = "fitted_vs_actual"}).
#'
#' @param x A bundle, \code{ild_diagnostics} object, or a fitted model with \code{ild_data}.
#' @param type When \code{x} is \code{lmerMod} or \code{lme}: passed to [ild_plot()]
#'   (default \code{"fitted_vs_actual"}). When \code{x} is \code{brmsfit}: \code{"pp_check"}
#'   or \code{"fitted_vs_actual"}. When \code{x} is \code{\link{ild_diagnostics_bundle}}:
#'   plot \code{type} within \code{section} — for \code{section = "residual"} one of
#'   \code{"acf"}, \code{"qq"}, \code{"fitted"}; other sections use a single type each
#'   (\code{"convergence"}, \code{"ppc"}, \code{"missingness"}, \code{"coverage"},
#'   \code{"weights"}). Use \code{type = NULL} with \code{section = "residual"} and
#'   legacy residual diagnostics to obtain the full named list from [plot_ild_diagnostics()];
#'   otherwise \code{NULL} picks a default per section.
#' @param section For \code{\link{ild_diagnostics_bundle}} only: \code{"residual"},
#'   \code{"fit"}, \code{"predictive"}, \code{"data"}, \code{"design"}, or \code{"causal"}.
#' @param ... Passed to [plot_ild_diagnostics()] (\code{ild_diagnostics} objects, or bundle
#'   \code{section = "residual"} with \code{type = NULL} and legacy diagnostics), to
#'   [ild_plot()] (\code{lmerMod}/\code{lme}), or to \code{brms::pp_check()} (bundle
#'   \code{section = "predictive"}, \code{type = "ppc"}).
#' @export
ild_autoplot <- function(x, ...) {
  UseMethod("ild_autoplot")
}

#' @rdname ild_autoplot
#' @method ild_autoplot ild_diagnostics
#' @export
ild_autoplot.ild_diagnostics <- function(x, ...) {
  plot_ild_diagnostics(x, ...)
}

#' @rdname ild_autoplot
#' @method ild_autoplot lmerMod
#' @export
ild_autoplot.lmerMod <- function(x, type = "fitted_vs_actual", ...) {
  ild_plot(x, type = type, ...)
}

#' @rdname ild_autoplot
#' @method ild_autoplot lme
#' @export
ild_autoplot.lme <- function(x, type = "fitted_vs_actual", ...) {
  ild_plot(x, type = type, ...)
}

#' @rdname ild_autoplot
#' @method ild_autoplot default
#' @export
ild_autoplot.default <- function(x, ...) {
  stop(
    "No ild_autoplot method for class ", paste(class(x), collapse = ", "),
    ". Use plot_ild_diagnostics() or ild_plot().",
    call. = FALSE
  )
}
