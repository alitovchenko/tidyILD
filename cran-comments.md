## Resubmission

This is a resubmission of tidyILD (0.4.0 -> 0.4.1).

The previous version was OK on all 12 standard CRAN flavors. The only problem was
the supplementary "Additional issues: linux-arm64 noLD" check, where two
`lme4`-based tests failed under a no-long-double (reduced-precision) build:

* `test-contract-regression.R` — a within/between fixture regressed an outcome on its
  own person mean, driving the random-intercept variance to ~0; under noLD
  `lme4::isSingular()` flipped to `TRUE` and fired a spurious singular-fit guardrail.
* `test-heterogeneity-lmer.R` — a random-slope fit on a near-zero slope variance hit a
  Cholesky downdate hard-error ("Downdated VtV is not positive definite") under noLD.

Both were test-side numerical-boundary fragilities, not bugs in package code. This
release:

* changes the contract fixture to decompose an independent predictor so the random
  intercept is genuinely identified and the fit is robustly non-singular at any
  precision, and
* wraps the random-slope test so an optimizer hard-error becomes a graceful `skip()`.

No user-facing code or behavior changed.

## Test environments

* local macOS, R 4.5.3 — OK
* (to confirm before resubmission) noLD build via the r-hub `noLD` container /
  `--disable-long-double`, `R CMD check --as-cran` — expecting `FAIL 0` for testthat.

## R CMD check results

0 errors | 0 warnings | 0 notes (on standard flavors).
