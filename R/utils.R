# Internal helpers for tidyILD (spacing stats, duplicate handling, etc.)
# Not exported.

#' Compute descriptive spacing stats from .ild_dt (excluding NA)
#' Returns list: median_dt, iqr_dt, n_intervals, pct_gap (if gap_threshold given), etc.
#' @noRd
ild_spacing_stats <- function(dt, gap_threshold = NULL) {
  dt <- dt[!is.na(dt)]
  n <- length(dt)
  if (n == 0) {
    return(list(
      median_dt = NA_real_,
      iqr_dt = NA_real_,
      n_intervals = 0L,
      pct_gap = NA_real_
    ))
  }
  median_dt <- stats::median(dt)
  iqr_dt <- stats::IQR(dt)
  pct_gap <- if (!is.null(gap_threshold) && is.finite(gap_threshold)) {
    mean(dt > gap_threshold) * 100
  } else {
    NA_real_
  }
  list(
    median_dt = median_dt,
    iqr_dt = iqr_dt,
    n_intervals = n,
    pct_gap = pct_gap
  )
}

#' Convert time column to numeric (seconds from epoch) for .ild_time_num
#' Handles Date, POSIXct, POSIXlt, and numeric (passed through).
#' @noRd
ild_time_to_num <- function(t) {
  if (is.numeric(t)) return(as.numeric(t))
  if (lubridate::is.Date(t)) {
    return(as.numeric(as.POSIXct(t)))
  }
  if (lubridate::is.POSIXt(t)) {
    return(as.numeric(t))
  }
  # try parsing as character to POSIXct
  t <- tryCatch(lubridate::as_datetime(t), error = function(e) NULL)
  if (is.null(t)) stop("Time column could not be converted to numeric (Date/POSIXct/numeric).", call. = FALSE)
  as.numeric(t)
}
