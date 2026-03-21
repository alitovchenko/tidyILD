# Umbrella documentation: standalone diagnostics utilities and bundle section providers
# (see ?ild_diagnostics_utilities)

#' Standalone diagnostics utilities and bundle section providers
#'
#' @description
#' These functions are **standalone utilities** you can run on prepared ILD data at any time
#' (typically before or alongside modeling). The same logic also **feeds**
#' \code{\link{ild_diagnostics_bundle}} sections when you call \code{\link{ild_diagnose}} on a
#' fitted model, so the package presents one coherent diagnostics story: a top-level façade
#' (\code{ild_diagnose}) plus reusable building blocks below.
#'
#' @section Relationship to \code{\link{ild_diagnose}}:
#' \describe{
#'   \item{\code{\link{ild_design_check}}}{Populates the bundle \strong{design} slot (embedded as
#'     \code{design$ild_design_check}) and contributes WP/BP and design-time missingness views.}
#'   \item{\code{\link{ild_missing_pattern}}}{Drives \strong{data} (\code{data$missing_pattern},
#'     global columns) and \strong{missingness} (variable-specific summaries when model predictors
#'     are known).}
#'   \item{\code{\link{ild_missing_model}}}{Not called automatically by \code{ild_diagnose}.
#'     Use when you fit a missingness model for IPW or interpretation; it is the usual
#'     prerequisite for \code{\link{ild_ipw_weights}}.}
#'   \item{\code{\link{ild_ipw_weights}}}{Adds \code{.ipw} (and related steps in the IPW workflow).
#'     When those columns are present on the ILD data attached to the fit, \code{ild_diagnose}
#'     fills the bundle \strong{causal} slot with weight summaries.}
#' }
#'
#' @seealso \code{\link{ild_diagnose}}, \code{\link{ild_diagnostics_bundle}}, \code{\link{ild_diagnostics}}
#' @name ild_diagnostics_utilities
NULL
