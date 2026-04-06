#!/usr/bin/env Rscript
# Evaluate benchmark_summary.csv against thresholds JSON; exit 1 on hard failures.
# Usage:
#   Rscript scripts/check-backend-validation-thresholds.R \
#     --summary /path/benchmark_summary.csv \
#     --thresholds inst/benchmarks/thresholds-smoke.json \
#     --out /path/benchmark_checks.csv

`%||%` <- function(x, y) if (is.null(x)) y else x

args_cmd <- commandArgs()
file_arg <- grep("^--file=", args_cmd, value = TRUE)
script_dir <- if (length(file_arg)) dirname(sub("^--file=", "", file_arg[1L])) else "."
pkg_root_script <- dirname(script_dir)
owd <- normalizePath(getwd(), mustWork = FALSE)
pkg_root <- if (file.exists(file.path(owd, "DESCRIPTION"))) {
  owd
} else if (file.exists(file.path(pkg_root_script, "DESCRIPTION"))) {
  normalizePath(pkg_root_script, mustWork = FALSE)
} else {
  normalizePath(".", mustWork = FALSE)
}

harness_path <- file.path(pkg_root, "tests", "testthat", "helper-backend-validation-harness.R")
if (!file.exists(harness_path)) {
  stop("Harness helper not found at ", harness_path, call. = FALSE)
}
sys.source(harness_path, envir = .GlobalEnv)

args <- commandArgs(trailingOnly = TRUE)
summary_path <- NA_character_
thresholds_path <- NA_character_
out_path <- file.path(getwd(), "benchmark_checks.csv")
warn_only_global <- FALSE

i <- 1L
while (i <= length(args)) {
  a <- args[i]
  if (grepl("^--summary=", a)) {
    summary_path <- sub("^--summary=", "", a)
  } else if (a == "--summary" && i < length(args)) {
    summary_path <- args[i + 1L]
    i <- i + 1L
  } else if (grepl("^--thresholds=", a)) {
    thresholds_path <- sub("^--thresholds=", "", a)
  } else if (a == "--thresholds" && i < length(args)) {
    thresholds_path <- args[i + 1L]
    i <- i + 1L
  } else if (grepl("^--out=", a)) {
    out_path <- sub("^--out=", "", a)
  } else if (a == "--out" && i < length(args)) {
    out_path <- args[i + 1L]
    i <- i + 1L
  } else if (a == "--warn-only") {
    warn_only_global <- TRUE
  }
  i <- i + 1L
}

if (is.na(summary_path) || !nzchar(summary_path)) {
  stop("Provide --summary path to benchmark_summary.csv", call. = FALSE)
}
if (is.na(thresholds_path) || !nzchar(thresholds_path)) {
  stop("Provide --thresholds path to JSON", call. = FALSE)
}
if (!file.exists(summary_path)) {
  stop("Summary file not found: ", summary_path, call. = FALSE)
}
if (!file.exists(thresholds_path)) {
  thr_alt <- file.path(pkg_root, thresholds_path)
  if (file.exists(thr_alt)) {
    thresholds_path <- thr_alt
  } else {
    stop("Thresholds file not found: ", thresholds_path, call. = FALSE)
  }
}

summ <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
thr <- harness_read_thresholds_json(thresholds_path)

checks <- harness_evaluate_thresholds(summ, thr)
if (isTRUE(warn_only_global)) {
  checks$status[checks$status == "fail"] <- "warn"
}

utils::write.csv(checks, out_path, row.names = FALSE)

n_fail <- sum(checks$status == "fail", na.rm = TRUE)
n_warn <- sum(checks$status == "warn", na.rm = TRUE)
message("Threshold checks written to ", out_path, " fail=", n_fail, " warn=", n_warn)

status <- harness_threshold_exit_status(checks)
quit(status = status)
