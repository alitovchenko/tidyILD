#' Convert an ILD object to a tsibble
#'
#' Wraps [tsibble::as_tsibble()] using the subject and time column names from
#' [ild_meta()] as key and index. Use [ild_prepare()] with a \code{tbl_ts} input
#' (omitting \code{id} and \code{time}) for the reverse direction when
#' \pkg{tsibble} is installed.
#'
#' @param x An object that passes [validate_ild()].
#' @param ... Optional arguments passed to [tsibble::as_tsibble()].
#' @return A \code{tbl_ts} object (see \pkg{tsibble}).
#' @examples
#' \donttest{
#' d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' if (requireNamespace("tsibble", quietly = TRUE)) {
#'   ts <- ild_as_tsibble(x)
#'   class(ts)
#' }
#' }
#' @importFrom rlang sym
#' @export
ild_as_tsibble <- function(x, ...) {
  validate_ild(x)
  if (!requireNamespace("tsibble", quietly = TRUE)) {
    stop("Package 'tsibble' is required for ild_as_tsibble(). Install it with install.packages(\"tsibble\").", call. = FALSE)
  }
  m <- ild_meta(x)
  id_col <- m$ild_id
  time_col <- m$ild_time
  tsibble::as_tsibble(
    tibble::as_tibble(x),
    key = !!rlang::sym(id_col),
    index = !!rlang::sym(time_col),
    ...
  )
}
