#!/usr/bin/env Rscript
# Cross-backend validation benchmark runner (CI / local).
# Run from the package source root (where DESCRIPTION lives) so the harness helper is available.
# Usage:
#   Rscript scripts/run-backend-validation-benchmarks.R --tier smoke --out-dir /tmp/bench
# See inst/dev/BACKEND_VALIDATION_BENCHMARK_CONTRACT.md

`%||%` <- function(x, y) if (is.null(x)) y else x

args_cmd <- commandArgs()
file_arg <- grep("^--file=", args_cmd, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(sub("^--file=", "", file_arg[1L]))
} else {
  "."
}
pkg_root_script <- dirname(script_dir)

owd <- normalizePath(getwd(), mustWork = FALSE)
if (file.exists(file.path(owd, "DESCRIPTION"))) {
  pkg_root <- owd
} else if (file.exists(file.path(pkg_root_script, "DESCRIPTION"))) {
  pkg_root <- normalizePath(pkg_root_script, mustWork = FALSE)
} else {
  stop(
    "Run from package root (directory containing DESCRIPTION) or place DESCRIPTION next to scripts/.",
    call. = FALSE
  )
}

harness_path <- file.path(pkg_root, "tests", "testthat", "helper-backend-validation-harness.R")
if (!file.exists(harness_path)) {
  stop("Harness helper not found at ", harness_path, call. = FALSE)
}

loaded <- FALSE
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(pkg_root, quiet = TRUE)
  loaded <- TRUE
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(pkg_root, quiet = TRUE)
  loaded <- TRUE
}
if (!loaded) {
  suppressPackageStartupMessages(library(tidyILD))
}

sys.source(harness_path, envir = .GlobalEnv)

opt <- harness_parse_cli_args()
dir.create(opt$out_dir, recursive = TRUE, showWarnings = FALSE)

sha <- opt$git_sha
if (is.na(sha) || !nzchar(sha)) {
  sha <- Sys.getenv("GITHUB_SHA", unset = NA_character_)
}
if (is.na(sha) || !nzchar(sha)) {
  sha <- tryCatch(
    trimws(system2("git", c("-C", pkg_root, "rev-parse", "HEAD"),
      stdout = TRUE, stderr = FALSE
    )[1L]),
    error = function(e) "unknown"
  )
}

res <- harness_run_benchmark(
  tier = opt$tier,
  backends = opt$backends,
  n_sim = opt$n_sim,
  seed = opt$seed,
  run_id = opt$run_id,
  git_sha = sha,
  brms_iter = opt$brms_iter,
  brms_chains = opt$brms_chains,
  ctsem_iter = opt$ctsem_iter,
  ctsem_chains = opt$ctsem_chains
)

utils::write.csv(res$raw, file.path(opt$out_dir, "benchmark_raw.csv"), row.names = FALSE)
utils::write.csv(res$summary, file.path(opt$out_dir, "benchmark_summary.csv"), row.names = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  warning("jsonlite not installed; writing benchmark_metadata.rds only.", call. = FALSE)
  saveRDS(res$metadata, file.path(opt$out_dir, "benchmark_metadata.rds"))
} else {
  jsonlite::write_json(
    res$metadata,
    file.path(opt$out_dir, "benchmark_metadata.json"),
    auto_unbox = TRUE, pretty = TRUE
  )
}

message(
  "Benchmark finished: tier=", opt$tier,
  " rows_raw=", nrow(res$raw),
  " summary_groups=", nrow(res$summary),
  " out_dir=", normalizePath(opt$out_dir, mustWork = FALSE)
)
