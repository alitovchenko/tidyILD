# tidyILD 0.3.0

## Guardrails (identity & reporting)

- **`print()`** on **`ild_diagnostics_bundle`** prints a **Summary** after slots: counts for warnings/guardrails, highest guardrail severity, and up to five **`rule_id`** values when guardrails are present.
- **`ild_report()`** enriches **`diagnostics_summary`** with **`guardrails_narrative`** and a **`guardrails`** list (`n`, `max_severity`, `rule_ids`); when guardrails fired, **`methods_with_guardrails`** combines methods text with the guardrail sentence.
- **`ild_methods()`** accepts optional **`bundle`** (typically `ild_diagnose(fit)`) to append methodological cautions when guardrails triggered.

## Documentation

- **`ild_diagnose()` / `ild_diagnostics_bundle`:** sections are **denser by default**: `data` includes **`cohort`**, **`obs_per_id`**, **`outcome_summaries`**, **`missingness_rates`**; `design` includes **`flags`**, **`time_coverage`**, **`occasion_imbalance`** (mirrors `obs_per_id`); `fit` includes **`reml`**, **`optimizer`**, **`theta_summary`**, **`X_rank`**, **`residual_correlation`** (frequentist/brms); **`predictive`** adds **`n`**, **`mean_residual`**, **`max_abs_error`**; **`missingness`** is always structured (`note`, `pct_na_by_var`). Optional **`missing_model`** / **`missing_model_predictors`** / **`causal_detail`** gate **`ild_missing_model()`** and extra IPW quantiles.
- **`ild_autoplot()`** for **`ild_diagnostics_bundle`** is **section-first**: use `section` and `type` (e.g. `residual` + `acf` / `qq` / `fitted`; `fit` + `convergence`; `predictive` + `ppc` for brms; `data` + `missingness`; `design` + `coverage`; `causal` + `weights`). With `section = "residual"` and `type = NULL`, legacy bundles still return the full named list from `plot_ild_diagnostics()` when `residual$legacy_ild_diagnostics` is present. **`ild_diagnose()`** sets `attr(bundle, "ild_fit")` and `attr(bundle, "ild_data")` for plotting.
- **`?ild_diagnostics_utilities`**: umbrella help topic describing **standalone** use and **bundle section** roles for `ild_design_check()`, `ild_missing_pattern()`, `ild_missing_model()`, and `ild_ipw_weights()` alongside `ild_diagnose()` / `ild_diagnostics_bundle`.
- **Developer package standards:** normative spec in **`inst/dev/DEVELOPER_CONTRACTS.md`** and vignette **`vignette("developer-contracts", package = "tidyILD")`** (diagnostics bundle sections, tidy/augment semantics, backend adapter checklist).

## `ild_tidy()` schema migration

- **`tidy_ild_model()`** and **`ild_tidy.brmsfit()`** now emit **all** columns from **`ild_tidy_schema()`**: `conf_low` / `conf_high` (replacing **`ci_low`** / **`ci_high`**), plus **`component`** (currently `"fixed"` for coefficient rows), **`effect_level`** (conservative: `"population"` for intercept, `"within"` / `"between"` for `_wp` / `_bp` terms, `"cross_level"` for interactions with both, `"unknown"` otherwise), **`statistic`**, **`interval_type`** (`"Wald"` frequentist, `"quantile"` brms), **`engine`**, **`model_class`**. Optional columns are included (`NA` for frequentist; **brms** fills **rhat** / **ESS** when `intervals = TRUE`).
- Internal helpers in **`ild_tidy_helpers.R`**; see **`vignette("developer-contracts", package = "tidyILD")`** for semantics.

## `ild_augment()` schema migration

- **`augment_ild_model()`** and **`ild_augment.brmsfit()`** emit all **`ild_augment_schema()`** required columns: **`.outcome`** (canonical observed vector; formula-named response column removed), **`.resid_std`** (Pearson-type via `residuals(..., type = "pearson")` when length-matched; otherwise **`NA`**), **`engine`**, **`model_class`**, plus optional columns (mostly **`NA`** until used).
- **brms:** posterior interval columns are **`.fitted_lower`** / **`.fitted_upper`** (replacing **`.fitted_q2.5`** / **`.fitted_q97.5`**).
- Helpers in **`ild_augment_helpers.R`**.

## Guardrails (analysis safety layer)

- **`guardrail_registry()`**: catalog of methodological rules (`rule_id`, `section`, `severity`, default messages). **`ild_diagnose()`** populates **`ild_diagnostics_bundle$guardrails`** with **triggered** rules only, using the canonical columns above. Rules cover mixed WP/BP predictors, irregular spacing without residual correlation, high gap rates, late missingness concentration, singular fits, poor mixing / low ESS (Bayesian), and unstable IPW weights.

## Contracts (Phase 1 — specifications only)

- **`ild_diagnostics_bundle()`**: canonical class for engine-agnostic diagnostics with fixed slots: `meta`, `data`, `design`, `fit`, `residual`, `predictive`, `missingness`, `causal`, `warnings`, `guardrails`, `summary_text` (see `?ild_diagnostics_bundle`). Populated by engines in a later phase.
- **`ild_tidy_schema()`** / **`ild_augment_schema()`**: documented required and optional column names; **`ild_tidy()`** and **`ild_augment()`** outputs match these contracts in this release.

## Provenance and reporting

- **Provenance schema:** Added `schema_version = "1"` and `object_type` to data and analysis provenance for future evolution. Step records include `step_id`.
- **ild_report()** now returns a stable schema: `meta` (n_obs, n_id, engine), `methods`, `model_table`, `diagnostics_summary`, `provenance`, `provenance_export_path`.
- **ild_methods()** is richer: row counts (N persons, n observations) when available, explicit IPW/weighted-estimation wording, diagnostics sentence reflects requested types (autocorrelation, Q-Q, etc.). New optional argument `robust_se` so methods text can mention cluster-robust SEs when used.
- **Provenance vignette:** *Reproducible ILD workflows with tidyILD provenance* (prepare, center, lag, fit, diagnostics, ild_history, ild_methods, ild_report, export, ild_compare_pipelines).
- **Analysis provenance** attached to `ild_crosslag()` and `ild_ipw_refit()` return values.
- **Package description** updated to "A reproducible, tidyverse-style framework for intensive longitudinal data analysis in R, with built-in methodological safeguards, provenance tracking, and reporting tools."

## Unified API and tsibble

- **ild_lme()** analysis provenance records `backend` (`"lme4"` / `"nlme"`), `fit_engine` (`"lmer"` / `"lme"`), and `backend_version` (installed package version).
- **ild_fit()** wrapper to choose `backend = "lme4"` or `"nlme"` (same as `ild_lme()` with `ar1` implied).
- **ild_tidy()**, **ild_augment()**, **ild_autoplot()** S3 generics dispatching to `tidy_ild_model()`, `augment_ild_model()`, and `plot_ild_diagnostics()` / `ild_plot()` as appropriate.
- **ild_as_tsibble()** converts an ILD object to a `tbl_ts` (optional **tsibble** in Suggests). **ild_prepare()** with both `id` and `time` omitted can take a `tbl_ts` when `tsibble` is installed (single key + index inferred).

## Bayesian backend (brms)

- **ild_brms()** fits Bayesian mixed models via **brms**, with **ild_prior_ild()** templates (`default`, `weakly_informative`, `minimal_shrinkage`). Attributes **ild_posterior** (sampler settings, prior summary, divergences) and **ild_provenance** are attached.
- **ild_tidy()** / **ild_augment()** methods for `brmsfit` (posterior means, 95% intervals, R-hat, ESS; augmented fitted intervals as `.fitted_lower` / `.fitted_upper`).
- **ild_diagnose()** always returns **`ild_diagnostics_bundle`** for `lmerMod`, `lme`, and `brmsfit` (unified slots: data, design, fit, residual, predictive, missingness, causal, warnings, guardrails, `summary_text`). Frequentist residual plots use **`residual$legacy_ild_diagnostics`** with **`plot_ild_diagnostics()`** or **`ild_autoplot(bundle, section = "residual")`**. Call **`ild_diagnostics()`** directly if you only need the legacy object. **ild_methods()** and **ild_report()** describe priors, chains, warmup, and sampler controls for `ild_brms` fits.

# tidyILD 0.2.0

This release builds on four pillars: **methodological safeguards**, **modeling breadth**, **provenance tracking**, and **reporting tools**.

## Safeguards

- **Robust uncertainty:** `ild_robust_se()` provides cluster-robust variance estimators (CR0/CR2/CR3) via clubSandwich. `tidy_ild_model()` gains `se = "robust"` and `robust_type`; CIs and p-values use Wald normal approximation when using robust SEs.
- **Missingness and IPW:** `ild_missing_model()` fits missingness models (glm/glmer); `ild_ipw_weights()` generates inverse probability weights (stabilized or not, optional trim); `ild_ipw_refit()` refits ild_lme with IPW (lmer only). Documented as diagnostic/sensitivity tooling, not a full MNAR solution.
- **Design and diagnostics:** `ild_design_check()` aggregates spacing, WP/BP decomposition, and missingness with recommendations. `ild_spacing()` reports interval stats and AR1/CAR1 recommendation. `ild_missing_bias()` tests whether missingness is associated with a predictor. `ild_center_plot()` for WP vs BP density. WP/BP safeguard warning in `ild_lme()` when predictors vary at both levels (suggests `ild_center()`).

## Modeling breadth

- **Time-varying effects:** `ild_tvem()` fits time-varying effect models using mgcv; `ild_tvem_plot()` visualizes the coefficient curve with confidence band.
- **Power analysis:** `ild_power()` for simulation-based power of a fixed effect (ild_simulate -> ild_lme -> effect recovery); supports lmer and nlme.
- **Cross-lag and person-level:** `ild_crosslag()` one-call pipeline (ild_lag -> ild_check_lags -> ild_lme). `ild_person_model()` and `ild_person_distribution()` for per-person fits and estimate distribution.
- **Multi-stream alignment:** `ild_align()` aligns a secondary stream to primary ILD within a time window (e.g. self-report + wearables).
- **Visualization:** `ild_heatmap()`, `ild_spaghetti()`, `ild_circadian()` (time-of-day patterns when time is POSIXct).

## Provenance

- **Data and analysis provenance:** Preprocessing steps (ild_prepare, ild_center, ild_lag, ild_align, ild_ipw_weights) and analysis steps (ild_lme, ild_diagnostics, ild_tvem, ild_power, ild_missing_model, ild_crosslag, ild_ipw_refit) are recorded with a stable schema (version, schema_version, object_type, step_id, args, outputs).
- **ild_provenance()** and **ild_history()** return or print the recorded steps. **ild_export_provenance()** writes provenance to JSON or YAML for reproducibility supplements and archiving.
- **ild_compare_pipelines()** compares two ILD or model objects and reports differing steps and arguments.

## Reporting

- **ild_methods()** generates a methods-style narrative from provenance (row counts, AR1/CAR1, created variables, IPW and robust SE when specified). Optional `robust_se` argument for cluster-robust SE mention.
- **ild_report()** returns a standardized list: `meta` (n_obs, n_id, engine), `methods`, `model_table`, `diagnostics_summary`, `provenance`, `provenance_export_path`. Optional provenance export in one call.
- New vignette: *Reproducible ILD workflows with tidyILD provenance* (ild_history, ild_methods, ild_report, export, compare).

## Other improvements

- **Model tidiers:** `augment_ild_model()` and `tidy_ild_model()` with consistent columns across lmer/nlme; S3 print methods.
- **Documentation:** Package described as a reproducible, tidyverse-style framework with safeguards, provenance, and reporting. Vignettes use `set.seed()` / `seed` for determinism; optional-package examples (e.g. clubSandwich) use `eval = requireNamespace(...)`. `ild_power()` examples kept small (n_sim = 25).

# tidyILD 0.0.1 (initial release)

- Initial CRAN release.
- Pipeline: `ild_prepare()`, `ild_summary()`, `ild_center()`, `ild_lag()` (index, gap-aware, time-window), `ild_spacing_class()`, `ild_missing_pattern()`, `ild_check_lags()`.
- Modeling: `ild_lme()` (lmer or nlme with AR1/CAR1), `ild_diagnostics()`, `ild_plot()` (trajectory, gaps, missingness, fitted, residual ACF).
- Utilities: `ild_simulate()`, `ild_manifest()` / `ild_bundle()` for reproducibility, broom integration for `ild_lme` fits.
- Data: `ema_example` dataset.
- Vignettes: workflow, within-between decomposition and spacing, glossary and quick-start.
