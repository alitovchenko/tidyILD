# tidyILD 0.2.0

## Major additions

- **Robust uncertainty**
  - New function `ild_robust_se()` providing cluster-robust variance estimators (CR0/CR2/CR3) using the clubSandwich package.
  - `tidy_ild_model()` gains `se = "robust"` and `robust_type` arguments; CIs and p-values use Wald normal approximation when using robust SEs.

- **Missingness and IPW diagnostics**
  - `ild_missing_model()` fits models for missingness (glm/glmer) and returns fit, tidy table, and predicted probabilities.
  - `ild_ipw_weights()` generates inverse probability weights (stabilized or not, with optional trim).
  - `ild_ipw_refit()` refits ild_lme models using IPW (lmer backend; nlme not supported). Documented as diagnostic/sensitivity tooling, not a full MNAR solution.

- **Time-varying effects**
  - `ild_tvem()` fits time-varying effect models using mgcv (s(time) + s(time, by = predictor) + optional random intercept by person).
  - `ild_tvem_plot()` visualizes the time-varying coefficient curve with confidence band.

- **Power analysis**
  - `ild_power()` for simulation-based power of a fixed effect (ild_simulate -> ild_lme -> effect recovery). Supports lmer and nlme; lmerMod inference uses Wald z-approximation when p-values are not provided by the backend.

- **Design and diagnostics**
  - `ild_design_check()` aggregates spacing, WP/BP decomposition, and missingness with a custom print method.
  - `ild_spacing()` reports interval stats (median, IQR, large gaps %, CV) and AR1/CAR1 recommendation.
  - `ild_missing_bias()` tests whether missingness is associated with a predictor.
  - `ild_center_plot()` for standalone WP vs BP density plot.

- **Cross-lag and person-level**
  - `ild_crosslag()` one-call pipeline: ild_lag -> ild_check_lags -> ild_lme; returns fit, lag coefficient, and optional diagnostics.
  - `ild_person_model()` and `ild_person_distribution()` for per-person fits and visualization of estimate distribution.

- **Visualization**
  - `ild_heatmap()` and `ild_spaghetti()` as thin aliases for `ild_plot(type = "heatmap")` and `ild_plot(type = "trajectory")`.
  - `ild_circadian()` for time-of-day patterns (when time is POSIXct).
  - `ild_align()` for multi-stream alignment within a time window (e.g. self-report + wearables).

## Other improvements

- **Model tidiers:** `augment_ild_model()` and `tidy_ild_model()` with consistent columns across lmer/nlme; S3 print methods for diagnostics and tidy model object.
- **Documentation:** Package help and vignettes updated; pkgdown site; short analysis-report vignette with robust SE, TVEM, and AR1 examples.
- **CRAN:** Examples use `set.seed()` for determinism; `ild_power()` examples kept small (n_sim = 25); nlme example in ild_lme wrapped in `\dontrun{}` where convergence is platform-sensitive.

# tidyILD 0.0.1 (initial release)

- Initial CRAN release.
- Pipeline: `ild_prepare()`, `ild_summary()`, `ild_center()`, `ild_lag()` (index, gap-aware, time-window), `ild_spacing_class()`, `ild_missing_pattern()`, `ild_check_lags()`.
- Modeling: `ild_lme()` (lmer or nlme with AR1/CAR1), `ild_diagnostics()`, `ild_plot()` (trajectory, gaps, missingness, fitted, residual ACF).
- Utilities: `ild_simulate()`, `ild_manifest()` / `ild_bundle()` for reproducibility, broom integration for `ild_lme` fits.
- Data: `ema_example` dataset.
- Vignettes: workflow, within-between decomposition and spacing, glossary and quick-start.
