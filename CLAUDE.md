# CLAUDE.md — mocaredd.dev2

Development version of {mocaredd} (openforis/mocaredd): an R package +
Shiny app for Monte Carlo uncertainty propagation in REDD+ carbon
accounting (FAO, AIM4Forests). Maintainer: Gaël Sola. Users: country MRV
teams (semi-technical ministry/agency staff).

## Hard rules

- R only. No Python scripts as deliverables. Base R + tidyverse-style
  packages already in DESCRIPTION.
- Never hand-edit `NAMESPACE` or `man/`. Edit roxygen, then
  `devtools::document()`.
- New dependencies: `usethis::use_package()`, call as `pkg::fun()`.
  Don’t add heavy deps without asking.
- All user-facing UI strings go through `i18n$t()` and
  `inst/assets/translations.json`.
- Keep text short: comments, roxygen, messages and explanations to me.
  No filler.
- Don’t change numerical results silently. If a change alters ERR, E or
  uncertainty values on the test workbooks, say so and show
  before/after.

## Dev loop

``` r

devtools::load_all()
devtools::document()
shiny_run_mocaredd_dev2()          # launch the app
devtools::check()                  # before committing
```

Quick end-to-end in the console:

``` r

path    <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
checked <- fct_checkinput(.path = path)
ari     <- fct_arithmetic_mean2(.checked_data = checked)   # Approach 1 / arithmetic
sims    <- fct_combine_mcs_E(.checked_data = checked)      # MC per transition
sa      <- fct_sensitivity2(.checked_data = checked)
```

There is no testthat suite yet (`tests/tuto-pkg.R` is a setup notebook,
not tests). Adding `tests/testthat/` with regression tests on the
extdata workbooks is welcome.

## Code map

**Active app path (v2)** — wired in `R/shiny_run_mocaredd_dev2.R`:

- UI/server modules: `mod_home_UI2` / `mod_home_server2`, `mod_tool_UI2`
  / `mod_tool_server2`, `mod_about_*`.
- Shared state: one `rv` reactiveValues list (`checks`, `inputs`,
  `sims`, `res`, `gt`, `histo`, `actions`) passed to all modules.
  `rv$checked` holds the full
  [`fct_checkinput()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_checkinput.md)
  output.
- `inst/assets` is served at `assets/` via `.onLoad()` in `R/zzz.R`.

**Calculation chain** (used by `mod_tool_server2`):

1.  [`fct_checkinput()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_checkinput.md)
    reads and validates the xlsx, detects `template_version` (1 or 2),
    returns `$data` with `setup`, `time`, `area`, `carbon`. Every
    downstream function takes `.checked_data`.
2.  [`fct_arithmetic_mean2()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_arithmetic_mean2.md):
    deterministic means and IPCC Approach 1 propagation.
3.  [`fct_combine_mcs_E()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_combine_mcs_E.md):
    simulates CF, carbon elements, AD; builds C stocks (incl. DG_ratio),
    EF = C_i − C_f, E = AD × EF per transition and simulation.
4.  [`fct_combine_mcs_P()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_combine_mcs_P.md):
    aggregates to periods (reference / monitoring).
5.  [`fct_combine_mcs_ER()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_combine_mcs_ER.md):
    emission reductions.
6.  [`fct_calc_res()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_calc_res.md),
    [`fct_forestplot()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_forestplot.md),
    [`fct_histogram()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_histogram.md),
    [`fct_round()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_round.md):
    results and outputs.
7.  Helpers:
    [`fct_make_mcs()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_make_mcs.md)
    (PDF draws),
    [`fct_make_formula()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_make_formula.md),
    `fct_make_EF()`.

**Legacy v1** (not wired in the app; keep until replacement confirmed,
don’t extend): `mod_home_UI`, `mod_home_server`, `mod_tool_UI`,
`mod_tool_server`, `fct_check_data2`, `fct_arithmetic_mean`,
`fct_sensitivity` (unexported).

**Function conventions:** `fct_*` for functions, `mod_*` for modules,
arguments prefixed with a dot (`.checked_data`, `.n_iter`), native pipe
`|>`, `.data$col` inside dplyr,
[`rlang::.data`](https://rlang.r-lib.org/reference/dot-data.html)
imported.

## Domain essentials

- Emissions per transition: E = AD × EF. ERR = FREL (reference period
  mean) − monitoring emissions. Periods are weighted by length (1/n_REF
  vs 1/n_MON), so monitoring-period transitions carry more weight per
  unit of area uncertainty.
- Standards: IPCC Approach 1 (error propagation formulas) vs Approach 2
  (Monte Carlo). FCPF Carbon Fund and ART-TREES require Monte Carlo —
  the app must keep that path robust.
- Carbon: pools (AGB, BGB via RS, deadwood, litter, soil), CF when
  stocks are in dry matter, DG_ratio for degraded classes relative to
  their intact class (`lu_intact_*`, `c_intact_*`).

## Critical invariant: shared carbon draws

Carbon elements (and CF) are drawn once per iteration per land-use class
and reused across all transitions involving that class. This creates
structural correlation between transitions but the ~100 source inputs
(carbon elements + AD) stay mutually independent. Any refactor must
preserve this: drawing C stocks per transition would wrongly remove the
correlation and understate ERR uncertainty. Sensitivity methods must
perturb the source inputs, not transition-level EFs.

## Sensitivity analysis

Two tracks:

- `fct_sensitivity2()` (package): delta-method first-order contributions
  grouped by input type, plus DF/DG emissions split. Exported,
  documented in `vignettes/sensitivity-analysis.Rmd`. Not yet called
  from `mod_tool_server2` — wiring it into the Tool tab is an open task.
- `tests/sensitivity/sensitivity.R` (standalone, not a testthat test):
  runs the simulator once on the UGA template, then (A) grouped Shapley
  of `Var(ERR)` over five groups and (B) a tornado of every AD source.
  Supersedes the earlier `tests/test-sensitivity*.R`,
  `sensitivity_analysis.R`, `tornado_ad.R` and the out-of-repo
  `mocaredd_sensitivity_analysis.R` / `mocaredd_sensit_tornado_AD.R`
  scripts.

### Running the script

``` r

devtools::load_all()
source("tests/sensitivity/sensitivity.R")   # from the package root
```

Packages: `tidyverse`, `sensitivity`, `gtools`, `tictoc`, `scales`.
Settings in `cfg` at the top: `xlsx_path`
(`data-raw/UGA_mocareddv2_20260718.xlsx`, gitignored), `out_dir`
(`tests/sensitivity`), `n_iter` (10000, overrides the template), `seed`
(3980), `No_act` (300) and `Ni` (3) for `shapleyPermEx`, `top_n` (25).

Outputs in `tests/sensitivity/`:

| File | Content |
|----|----|
| `shapley_5groups.csv` | Sobol’ first/total and Shapley indices (95 % CI) for the five groups |
| `shapley_donut.png` | Donut of the five Shapley shares |
| `tornado_ad_table.csv` | All AD sources ranked: area, SE, EF, weight, contribution, low/high ERR, swing, share of total swing |
| `tornado_ad.png` | Tornado of the top `top_n` sources, coloured by DF/DG, baseline ERR marked |

The knn cross-check is printed only, not written.

### Model used by the script

`big <- fct_combine_mcs_E(checked)` gives realised `AD` and `EF` per
transition × simulation (plus `period_type`, `period_length`). The
script pivots them to a bank (`n_sim × 2·NT`) and evaluates

`ERR = Σ_t wt_t · AD_t · EF_t`, with
`wt_t = (+1/n_REF for REF, −1/n_MON for MON*, 0 for untyped gap periods) / period_length`.

`n_REF`, `n_MON` are numbers of periods, not years. Monitoring
transitions carry more weight when there are fewer MON periods.

### A. Grouped Shapley

| Group | Meaning |
|----|----|
| `AD_DF_REF` / `AD_DF_MON` | activity data, deforestation, reference / monitoring |
| `AD_DG_REF` / `AD_DG_MON` | activity data, degradation, reference / monitoring |
| `EF` | all emission-factor / carbon inputs, kept whole |

- EF is kept whole because of shared carbon draws: split by
  activity/period, groups correlate up to ~0.99 and the split is not
  identifiable.
- Independence is tested, not assumed: 5×5 correlation of group drivers
  (row means), typically `max |rho| ≈ 0.02`; flagged if ≥ 0.15.
- With independent groups, Shapley effects are exact and additive (sum
  to 100 %). They share the `AD × EF` interaction variance (ERR is
  bilinear), not dependence.
- Estimator: `sensitivity::shapleyPermEx()`, each group one “input” via
  a bank-row index, so a group is frozen/resampled as a block.
  `Nv = n_sim`. Cost `Nv + d!(d−1)·No·Ni`.
- Experimental check: `sobolshap_knn()` on all group subsets, Shapley
  assembled over permutations. Biased low for high-dimensional EF;
  shares usually don’t sum to 1.
- Expected on UGA: AD ≈ 0.92, EF ≈ 0.08; DF ≈ 0.91, DG ≈ 0.07, shared EF
  ≈ 0.02.

### B. AD tornado

Each AD source (`area` sheet: `trans_area`, `trans_se`) is swung across
`mean ± z·SE`, `z = qnorm(1 − (1 − conf_level)/2)` (0.9 → 1.645), all
else at baseline. ERR is linear in each AD, so
`swing_t = wt_t · EF_bar_t · 2z·SE_t`, with `EF_bar_t` the simulated
mean EF (baseline only). For AD, squared swing ranking = normalised
first-order Sobol’ = SensIt “Percent Swing²”; this breaks for EF/carbon
(shared draws).

### Open points

- Script not yet run since the merge; confirm it runs and regenerate
  outputs.
- Weights assume what UGA has (1-year periods, `ad_annual = TRUE`).
  Since
  [`fct_combine_mcs_E()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_combine_mcs_E.md)
  now always sets `period_length = nb_years`, check `wt_t` against
  [`fct_combine_mcs_P()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_combine_mcs_P.md)
  (which weights by years, and multiplies by `nb_years` when
  `ad_annual`) before using multi-year periods.
- Compare Part A with `fct_sensitivity2()` on the same workbook before
  integrating either into the app.
- `sensitivity` package versions: check `shapleyPermEx` /
  `sobolshap_knn` argument names.

References: Song, Nelson & Staum (2016) *SIAM/ASA JUQ* 4:1060–1083 ·
Owen (2014) *SIAM/ASA JUQ* 2:245–251 · Iooss & Prieur (2019) *IJUQ*
9:493–514 · Mara, Tarantola & Annoni (2015) *Environ. Model. Softw.*
72:173–183 · Saltelli et al. (2008, 2010) · Sobol’ (1993) · Jansen
(1999).

## Test data (`inst/extdata/`)

- `mocaredd-templatev2-simple.xlsx`, `mocaredd-templatev2-4pools.xlsx`:
  templates, default examples.
- Uganda case study (`UGA_mocareddv2_20260718.xlsx`,
  `LUS05-mocaredd-template-UGA.xlsx`) now lives in `data-raw/`
  (gitignored), not `inst/extdata/`.
- `example1-4pools.xlsx`, `example2-with-sims.xlsx` (+ `test-`
  variants): examples.
- `test-4pools-dupID/missingtab/missingcol.xlsx`,
  `test-simple-noCF.xlsx`: must fail/handle in checks.
- Not in repo: `mocareddCUAS3V2_d2.xlsx` (e-learning dataset, two
  forests, deforestation only, total carbon stocks). It contains a
  deliberate Forest1/Forest2 carbon inversion between the carbon block
  and the summary sheets — a teaching point; don’t “fix” it in that
  file.

## Known issues (spotted, not yet fixed)

- `inst/extdata/~$test-example2-with-sims.xlsx` is an Excel lock file
  committed by mistake.
- App footer is placeholder text (“MyApp”, “Your Name”, “XYZ
  Institute”).
- [`fct_make_mcs()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_make_mcs.md):
  “beta” uses `.params` only and ignores `.mean`/`.se`; no else branch
  for unsupported PDFs (returns an error on undefined `SIMS`). Output
  rounded to 3 decimals.
- `README.md` is a stub.
