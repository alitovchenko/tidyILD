# Methods narrative from provenance
# Uses ild_provenance(), ild_flatten_provenance() from ild_provenance.R

#' Turn one step record into a methods sentence (internal)
#' @noRd
ild_step_to_sentence <- function(s) {
  step <- s$step
  a <- s$args
  o <- s$outputs
  if (step == "ild_prepare") {
    gt <- if (is.numeric(a$gap_threshold) && is.finite(a$gap_threshold)) {
      paste(a$gap_threshold, "units")
    } else {
      "no gap threshold"
    }
    return(paste0(
      "Data were prepared using ild_prepare() with participant ID ", .def(a$id, "id"),
      ", time variable ", .def(a$time, "time"),
      ", and a gap threshold of ", gt, "."
    ))
  }
  if (step == "ild_center") {
    vars <- a$vars
    if (length(vars) > 1) vars <- paste(vars, collapse = ", ")
    type <- .def(a$type, "person_mean")
    return(paste0(
      "Predictor(s) ", vars, " were ",
      if (type == "grand_mean") "grand-mean" else "person-mean",
      " centered using ild_center()."
    ))
  }
  if (step == "ild_lag") {
    vars <- a$vars
    if (length(vars) > 1) vars <- paste(vars, collapse = ", ")
    n <- .def(a$n, 1)
    mode <- .def(a$mode, "index")
    max_gap <- a$max_gap
    if (is.null(max_gap) || (is.numeric(max_gap) && !is.finite(max_gap))) max_gap <- "none"
    return(paste0(
      "A ", mode, " lag of ", vars, " was computed using ild_lag() with lag ", n,
      " and max gap ", max_gap, "."
    ))
  }
  if (step == "ild_align") {
    return(paste0(
      "Variable ", .def(a$value_var, "value"), " was aligned from secondary data using ild_align() with window ", .def(a$window, "window"), "."
    ))
  }
  if (step == "ild_ipw_weights") {
    return("Inverse probability weights were added using ild_ipw_weights().")
  }
  if (step == "ild_lme") {
    ar1 <- if (isTRUE(a$ar1)) "enabled" else "disabled"
    return(paste0(
      "A mixed-effects model was fit using ild_lme() with formula ", .def(a$formula, "as specified"),
      " and AR1 ", ar1, "."
    ))
  }
  if (step == "ild_diagnostics") {
    return("Residual diagnostics were computed (ACF, residuals vs time, etc.).")
  }
  if (step == "ild_tvem") {
    return(paste0(
      "A time-varying effects model was fit using ild_tvem() for outcome ", .def(a$outcome, "outcome"),
      " and predictor ", .def(a$predictor, "predictor"), "."
    ))
  }
  if (step == "ild_power") {
    return(paste0(
      "Simulation-based power analysis was run with n_sim=", .def(a$n_sim, "?"), ", effect_size=", .def(a$effect_size, "?"), ", test_term=", .def(a$test_term, "?"), "."
    ))
  }
  if (step == "ild_missing_model") {
    preds <- a$predictors
    if (length(preds) > 1) preds <- paste(preds, collapse = ", ")
    return(paste0(
      "A missingness model was fit for outcome ", .def(a$outcome, "outcome"),
      " and predictors ", preds, "."
    ))
  }
  paste0("Step ", step, " was applied.")
}

# Default value helper (internal)
.def <- function(x, y) if (is.null(x)) y else x

#' Generate methods-style narrative from provenance
#'
#' Takes an ILD data object, a model fit, or a diagnostics object and produces
#' a concise methods-style paragraph based on the recorded provenance (data
#' preparation, centering, lagging, modeling, etc.).
#'
#' @param x An ILD object (see [is_ild()]), a model from [ild_lme()] or [ild_tvem()],
#'   a diagnostics object from [ild_diagnostics()], or another object with
#'   [ild_provenance()] (e.g. [ild_power()] result, [ild_missing_model()] result).
#' @param ... Unused.
#' @return A single character string (one or more sentences) suitable for a
#'   methods section. Use \code{cat()} or \code{print()} to display.
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' x <- ild_center(x, y)
#' ild_methods(x)
ild_methods <- function(x, ...) {
  prov <- ild_provenance(x)
  if (is.null(prov)) {
    return("No provenance recorded for this object.")
  }
  steps <- ild_flatten_provenance(prov)
  if (length(steps) == 0) {
    return("Provenance has no steps recorded.")
  }
  sentences <- vapply(steps, ild_step_to_sentence, character(1L))
  paste(sentences, collapse = " ")
}
