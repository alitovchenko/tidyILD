# tidyILD handoff document

This document is the single source of truth for anyone (agent or human) picking up tidyILD with no prior context. It describes what has been built, issues encountered and how they were fixed, and best practices for writing, deploying, and pushing code.

---

## 1. Package overview

**tidyILD** is an R package for intensive longitudinal data (ILD). It provides a tidy pipeline from raw data to mixed-effects models with explicit time structure, within-between decomposition, spacing-aware lags, and diagnostics.

| Item | Detail |
|------|--------|
| **Repository** | GitHub `alitovchenko/tidyILD` |
| **Install** | `remotes::install_github("alitovchenko/tidyILD")` |
| **R version** | Depends R (>= 4.0.0) |
| **Imports** | tibble, dplyr, lubridate, rlang, lme4, nlme, ggplot2 |
| **Suggests** | testthat, roxygen2, knitr, broom.mixed |
| **License** | MIT + file LICENSE (LICENSE is a minimal stub: YEAR and COPYRIGHT HOLDER only, per CRAN) |

---

## 2. ILD contract (critical for any new code)

Only the code below may create or change ILD structure. Do not invent new `.ild_*` columns or `ild_*` attributes elsewhere.

### Reserved columns

Must not be created outside the single constructor:

- `.ild_id`, `.ild_time`, `.ild_time_num`, `.ild_seq`, `.ild_dt`, `.ild_gap`

**Who sets them:** Only `R/ild-class.R` and `R/ild_prepare.R` (via `new_ild_df()`). No other file should add or modify these columns or ILD metadata.

### Metadata attributes

Attached by `new_ild_df()`; read via `ild_meta(x)`:

- `ild_id`, `ild_time`, `ild_gap_threshold`, `ild_n_units`, `ild_n_obs`, `ild_spacing`

### Class and detection

- **Data:** Class `ild_tbl`. Use `is_ild(x)` to check.
- **Models:** For **nlme** fits we append class `ild_lme`. For **lmerMod** we do *not* add `ild_lme` to the class (only set attributes), so `residuals()`/`fitted()` keep working.
- **"Is this from tidyILD?"**  
  - Data: `is_ild(x)`  
  - Model: `inherits(object, "ild_lme") || !is.null(attr(object, "ild_data"))`

---

## 3. What has been built (by layer)

### Core

- **R/ild-class.R:** `is_ild`, `validate_ild`, `ild_meta`, `new_ild_df`, `as_ild`, `restore_ild_attrs`
- **R/utils.R:** `ild_spacing_stats`, `ild_spacing_by_id`, `ild_time_to_num`, `ild_handle_duplicates` (internal, not exported)

### Phase 1 pipeline

- **R/ild_prepare.R:** `ild_prepare` (id, time, gap_threshold, duplicate_handling, collapse_fn); adds `.ild_*` and `ild_spacing` (overall + `by_id` tibble)
- **R/ild_summary.R**, **R/ild_center.R**, **R/ild_lag.R** (modes: index, gap_aware, time_window with window/resolution), **R/ild_missing_pattern.R**, **R/ild_spacing_class.R**

### Phase 2

- **R/ild_lme.R:** lmer (ar1 = FALSE) or nlme with AR1/CAR1 (ar1 = TRUE); attributes `ild_data`, `ild_ar1`; for nlme, class `ild_lme` appended
- **R/ild_diagnostics.R:** ACF, residuals vs fitted/time, Q-Q; `plot_ild_diagnostics()`
- **R/ild_plot.R:** trajectory, gaps, missingness heatmap, fitted, residual_acf
- **R/ild_simulate.R:** simple simulated ILD

### Utilities and data

- **R/ild_check_lags.R:** audit lag columns vs max_gap
- **R/ild_manifest.R:** `ild_manifest()`, `ild_bundle()` for reproducibility when saving results
- **R/broom.R:** `tidy.ild_lme`, `augment.ild_lme` (broom.mixed in Suggests)
- **R/data.R:** documentation for `ema_example`  
  Data: `data/ema_example.rda`; creation script: `data-raw/create_ema_example.R` (data-raw is in .Rbuildignore)

### Tests

- **tests/testthat/:** test files for each main function plus `test-pipeline.R`, `test-ild_lag_time_window.R`, `test-ild_check_lags.R`, `test-ild_manifest.R`
- **Run:** `devtools::load_all("."); testthat::test_dir("tests/testthat")` or via R CMD check

### Vignettes

- **vignettes/tidyILD-workflow.Rmd:** full pipeline and reproducibility
- **vignettes/ild-decomposition-and-spacing.Rmd:** centering and lags
- **vignettes/ild-glossary-and-quickstart.Rmd:** glossary and quick-start

### CRAN / inst

- **inst/CITATION:** uses `bibentry()` and `meta$Version` (no `packageVersion("tidyILD")`)
- **inst/WORDLIST:** EMA, ILD, tidyverse (for DESCRIPTION spell check)
- **NEWS.md:** changelog (0.0.1 initial release)
- **LICENSE:** minimal stub (YEAR, COPYRIGHT HOLDER)
- **DESCRIPTION:** Authors@R, Depends R (>= 4.0.0), License: MIT + file LICENSE

---

## 4. Issues we ran into and how they were fixed

| Issue | Fix |
|-------|-----|
| **lmerMod and ild_lme class** — Prepending `ild_lme` to the S4 class of lmer fits broke `residuals()`/`fitted()`. | Do not add `ild_lme` to the class for lmerMod; only set attributes `ild_data`, `ild_ar1`. For nlme (S3) append class `ild_lme`. Detection: `inherits(., "ild_lme") \|\| !is.null(attr(., "ild_data"))`. |
| **ild_check_lags return type** — `do.call(rbind, ...)` risked wrong types. | Build list of lists with correct types (e.g. as.integer), then one `as_tibble(do.call(rbind, ...))` and handle empty case. |
| **Missingness heatmap** — Implement without adding tidyr. | Build long-format manually (e.g. lapply + rbind) and use ggplot2::facet_wrap for multiple vars. |
| **ild_diagnostics roxygen** — `@importFrom nlme coef` failed (no such export). | Use `coef(cs, ...)` without namespace; add `@importFrom stats coef`. |
| **NAMESPACE** — Roxygen exported `broom_ild_lme` (doc placeholder). | Remove `@export` from that NULL doc and remove stray export from NAMESPACE. |
| **data/ and data-raw** — Script failed when `data/` did not exist. | Create `data/` and run data-raw script; keep `data-raw` in .Rbuildignore. |
| **ild_manifest git SHA** — `system2("git", ..., cwd = git_path)` is invalid (base R has no `cwd`). | Use `system2("git", c("-C", git_path, "rev-parse", "HEAD"), ...)`. |
| **ild_bundle example** — `ild_lme(y ~ 1, ..., ar1 = FALSE)` errors (lmer needs random effects). | Use `y ~ 1 + (1 \| id)` and break the example across two lines for Rd line width (<= 100 chars). |
| **R CMD check undefined globals** — `coef`, `rnorm`, `runif` flagged. | Add `@importFrom stats coef` (ild_diagnostics), `@importFrom stats runif rnorm` (ild_simulate); roxygenise. |
| **DESCRIPTION** — CRAN wanted Authors@R and no invalid "R" field. | Use `Authors@R: person(given = "Alex", family = "Litovchenko", role = c("aut", "cre"), email = "al4877@columbia.edu")`; remove `R: ">= 4.0.0"`; add `Depends: R (>= 4.0.0)`. |
| **CITATION when package not installed** — `packageVersion("tidyILD")` fails. | Use `meta$Version` with fallback (meta is in environment when readCitationFile sources inst/CITATION). |
| **Rd line width** — Example in ild_bundle.Rd exceeded 100 characters. | Break example in roxygen (e.g. assign prepared data to variable, then ild_lme call); roxygenise. |
| **downloads/ in tarball** — CRAN saw non-standard top-level "downloads". | Add `^downloads$` to .Rbuildignore so R CMD build excludes it. |
| **CITATION old-style** — personList() and citEntry() deprecated. | Rewrite inst/CITATION with `bibentry()` and `c(person("Alex", "Litovchenko"))`, header/footer inside bibentry. |
| **License stub invalid DCF** — Full MIT text in LICENSE is not DCF. | Replace LICENSE content with minimal stub: `YEAR: 2025` and `COPYRIGHT HOLDER: Alex Litovchenko`. |
| **Possibly misspelled words in DESCRIPTION** — EMA, ILD, tidyverse. | Add inst/WORDLIST with those three words, one per line. |

---

## 5. Best practices (writing, deploying, pushing)

### Commit and authorship

- Do **not** add `Co-authored-by: Cursor` or any cursoragent to commit messages.
- Before push, check: `git log -1 --format="%B"` and ensure no Co-authored-by line.
- Prefer **thematic commits** (e.g. one commit per feature or logical change).
- User is the sole author in history; disable any IDE setting that auto-adds co-author.

### Code and API

- Keep **core APIs unchanged**; add new behavior via new functions or optional arguments.
- Use **explicit arguments** with clear defaults; no hidden magic.
- Only `R/ild-class.R` and ild_prepare (via `new_ild_df`) set `.ild_*` columns and `ild_*` attributes.
- For new R code that uses `stats` (e.g. coef, runif, rnorm), add the appropriate `@importFrom stats ...` and run roxygenise.

### Building and checking

- Run **`R CMD build .`** then **`R CMD check tidyILD_0.0.1.tar.gz`** (check the **tarball**, not the source directory) to avoid NOTEs about .git and inst/doc.
- After adding or changing exported functions, run **`roxygen2::roxygenise()`** to update NAMESPACE and man/.
- Keep **DESCRIPTION Collate** in sync with the order of R files when adding new ones.
- Keep **Rd examples** under 100 characters per line (break in roxygen).

### Git and remote

- **Remote:** SSH `git@github.com:alitovchenko/tidyILD.git`. Push: `git push origin main`.
- Do **not** commit build artifacts: .Rcheck, ..Rcheck, *.tar.gz. The `downloads/` folder holds the tarball locally but is not committed; .gitignore has *.tar.gz, .Rbuildignore has ^downloads$ so the tarball is not included in the built package.

### CRAN

- **Submission tarball:** Build with `R CMD build .`, then copy `tidyILD_0.0.1.tar.gz` to `downloads/` for upload.
- **inst/CITATION** must work when the package is not installed (use `meta$Version`).
- **LICENSE** is minimal stub (YEAR, COPYRIGHT HOLDER) for "MIT + file LICENSE".
- New acronyms or package names in Description that get spell-checked: add to **inst/WORDLIST**.

---

## 6. Quick reference

| Task | Command |
|------|---------|
| Run tests | `devtools::load_all("."); testthat::test_dir("tests/testthat")` |
| Build | `R CMD build .` (from package root) |
| Check | `R CMD check tidyILD_0.0.1.tar.gz` |
| Regenerate docs | `roxygen2::roxygenise()` |

**Key files:** DESCRIPTION, NAMESPACE, R/*.R (Collate order), tests/testthat/test-*.R, vignettes/*.Rmd, inst/CITATION, inst/WORDLIST, LICENSE, .Rbuildignore, .gitignore.
