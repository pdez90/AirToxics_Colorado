# Reproducibility manifest

**Policy (adopted 2026-08-15): every manuscript number must be reproducible from primary
inputs. No hand-made or interactive intermediate is accepted.**

Last full re-run: **2026-08-20/21**, from raw CDPHE inputs with `CLEAN=1`. All stages
completed; 23 of 23 figure groups passed. Every number in the revised manuscript comes from
that run.

## One command

```bash
bash ~/Downloads/Suncor/rerun_pipeline/RUN_EVERYTHING.sh 2>&1 | tee ~/Downloads/Suncor/rerun_pipeline/logs/run_console.log
```

| stage | what | time |
|---|---|---|
| 0 | guard tests — time convention + P08 geometry | ~1 min |
| 1 | `RUN_ALL_from_raw.R` with `CLEAN=1` | 3.5 h |
| 2 | P09 / P10 plume simulations | < 1 min |
| 3 | `MAKE_FIGURES.R` — all figures, incl. group I (HYSPLIT) | 4.5 h |
| 4 | impact diagnostic | 1 min |

Stage 0 is a hard gate: if the guard tests fail nothing else runs. Stages 1–4 continue on
error and print a pass/fail summary. Only stage 1 quarantines intermediates (to
`quarantine_intermediates_<ts>/`), so re-running stages 2–4 alone is safe. Everything is
logged under `rerun_pipeline/logs/run_<timestamp>/`.

`RUN_ALL_from_raw.R` can still be run alone. `hrrr_hour_cache/` holds raw NOAA fields keyed
by UTC hour — an input cache, not a derived product — so a warm cache speeds re-runs without
compromising the from-raw guarantee.

## Time convention — read this before touching any timestamp

`Local_Time_MST` is **fixed MST, UTC−7 all year, no daylight saving.** Verified two ways:
every raw string carries the literal `-0700` in all 12 months, and crews start at a fixed
*civil* hour, so on a true-MST clock the day's first record falls an hour earlier during
daylight-saving months — measured across the 101 sampling days at **0.95 h (95% CI
0.60–1.30)**, consistent with 1.00 h (p = 0.77) and rejecting 0.00 h (p ≈ 1e-6).

`date` is therefore a **fixed-MST wall clock stored with a UTC attribute — not an absolute
UTC instant.** The tzone attribute is a carrier for the clock reading, not a claim about the
instant. Two consumers depend on exactly this:

- `06_merge_with_wind.R` joins to EPA AQS `Date.Local`/`Time.Local`, which AQS publishes in
  Local Standard Time — the same clock. Parsing `date` as `America/Denver` instead makes it
  an instant 6–7 h from the AQS clock reading; the join still *succeeds*, silently pairing
  every mobile record with wind measured 6 h later in summer and 7 h later in winter.
- `P04` and `H04` need a true instant for HRRR and get it with
  `round → force_tz("MST") → with_tz("UTC")`. `force_tz` **asserts** the reading; it does not
  convert it.

Never use `America/Denver` on a pipeline timestamp. `tests/test_time_convention.R` enforces
this mechanically, including a static scan for any live `America/Denver` conversion.

## Tests

```bash
Rscript tests/test_p08_geometry.R
SUNCOR_CSV_DIR=Updated/csv SUNCOR_BASE=~/Downloads/Suncor Rscript tests/test_time_convention.R
SUNCOR_BASE=~/Downloads/Suncor Rscript tests/impact_of_time_fixes.R
```

Both tests exit non-zero on failure. `SUNCOR_STRICT=1` (which the driver sets) turns "no data
found, skipping" into a failure — a gate that passes because it found nothing to check is
worse than no gate. See `tests/README.md`.

`test_p08_geometry.R` is a closed loop: it forward-models a plume of known strength at
receptors placed off-centreline the way the ±10° acceptance window does, then inverts with
P08's own function. 540 cases, recovery exact to 7e-14 %.

## Primary inputs (the only accepted data)

| Input | Location | Source |
|---|---|---|
| Mobile air-toxics quarterly packets (20) | `Updated/*.xlsx` | **Official CDPHE repository** (colorado.gov/airquality/air_toxics_repo.aspx): 2023 Q1–2024 Q2 = _r3, 2024 Q3–2025 Q2 = _r2 — verified exact match to the posted revisions (R00a). 2025 Q3 posted but outside study period. |
| Mobile monthly CSVs (58) | `Updated/csv/{Suncor,Terminal}_<Month>_<Year>.csv` | Derived from the packets above (what script 02 reads). Coverage verified complete Feb 2023–Jun 2025 (R00a); content check vs xlsx via `DEEP=1`; regenerable via `REBUILD=1`. |
| Methane deployment CSVs (299) | `~/Toxics_EST/MethaneData/<quarter>/` | CDPHE direct — the only input NOT from the public repository; Picarro, uncalibrated. M01 truncates to the study period (≤ 2025-06-30) by default. |
| Hourly wind | `hourly_WIND_2023/2024/2025.csv` | EPA AQS Air Data (Local Standard Time) |
| La Casa stationary | `ascent_2023.csv`, `ascent_2024.csv`, `lacasa3.csv` | CDPHE / ASCENT |
| AirToxScreen 2020 | `airtoxscreen.xlsx` (+ `.csv` for Population) | EPA |
| TRI | `TRI.csv` | EPA |
| Census blocks | fetched live (`tigris::blocks("08", 2020)`) | US Census |
| HRRR meteorology | fetched live via Herbie (AWS), cached in `hrrr_hour_cache/` | NOAA |
| Grids (500 m, 5 km) | generated from scratch by `R00b_make_grids.R` (UTM 13N, cells snapped to absolute multiples over domain corners −105.25..−104.70, 39.60..40.00). No legacy shapefile used. | fully derived |

## Pipeline DAG (each stage = one wrapper, own R process, with diagnostics)

R00a verify inputs → R00b grids → R01 (raw CSVs → delay-corrected 1-s `mobile.RData`) →
R02 (wind merge) → R03 (background + 500 m segments; Fig 2) → R04 (La Casa scaling → census
blocks) → **R04b (canonical block risk; sources script 20)** → R05 (source-probability maps +
hotspot groups; Figs 3–4) → R06 (HRRR + WWTF + stability; P01–P06) → R07 (H2S plume ID +
Gaussian inversion; P07–P08) → M01–M04 (methane) → R99 (numbers report).

Then `MAKE_FIGURES.R` for the figure groups, including group I (script 36, HYSPLIT).

## Canonical definitions (locked in for this revision)

1. **Delays**: measured, asset-specific (CAT: aromatics 4 s, HCN 6 s, H2S 21 s; EMU: 5/3/17 s).

2. **Block-level benzene risk** = population-weighted, using
   `sBenzene_med_of_daily_med_scaled` (median of daily medians, La Casa bin-weighted scaling
   ×1.149), on blocks with AirToxScreen benzene + population > 0. Implemented in R04b +
   script 20. **This replaces the manuscript's irreproducible Feb-2026 numbers** (1,120
   blocks / 2.4×): that aggregation was interactive, left no code, and no tested
   reconstruction (10 candidates) reproduces it.

   Reproducible result, 2026-08-21 run: **1,668 blocks, 126,607 residents**; AirToxScreen
   0.117–0.416 excess cases, mobile 0.108–0.384, **ratio 0.92**. Per block, mobile benzene is
   *lower* than AirToxScreen in **77%** of blocks (median ratio 0.77), ≥2× higher in 6%, and
   an order of magnitude higher in **3** blocks. Block-level correlation is nil (Pearson
   0.004, Spearman −0.040). Framing: the two datasets disagree in **spatial pattern**, not in
   overall level — screening models approximate the aggregate but misplace it spatially.

3. **Plume inversion**: WWTP-updated funnel, **33 candidate events → 3 retained**. Baseline
   intercepts 471 / 1,003 / 1,964 metric t/yr; mean **1,146 metric t/yr, 95% CI −735 to
   3,027** — with n = 3 the interval spans zero. Well-posed scenario means 415–1,774 metric
   t/yr; 2 of 28 scenarios (`wd+10`, `avg_1s`) are flagged `usable = FALSE` because the
   receptor lies beyond 2σy, where the inferred rate is governed by the Gaussian tail rather
   than by the measurement. The manuscript's "137 candidates / seven retained" traces to a
   pre-WWTP analysis vintage.

   **Emission column**: the metric-tons/year column is `tpy_metric`. `Q_ppm_m3_s` is a legacy
   volumetric quantity and `kg_s` is a rate — neither is t/yr. Use
   `pick_emission_col()` (in `diagnostics_helpers.R`); do not pattern-match column names.

4. **Hotspots**: **18** persistent multi-pollutant groups (was 17 as submitted).

5. **Cadence**: native-cadence averaging (H2S 5 s, HCN 2 s, CH4 5 s), block mean assigned only
   to seconds that already held a value, applied after delay correction. Plume detection is
   exempt and uses the delivered `*_raw` signal.

## What the 2026-08-21 re-run changed

| item | as submitted | this run |
|---|---|---|
| 1-s measurements | 2,708,051 | 2,602,928 |
| 99th pct H2S / HCN | 5 / 12 ppb | 4.6 / 11 ppb |
| median H2S | 0 ppb | 0.2 ppb |
| census blocks ≥1 point | 2,832 | 2,857 |
| common blocks / residents | 1,120 / 83,828 | 1,668 / 126,607 |
| mobile : AirToxScreen risk ratio | 2.4 | 0.92 |
| persistent hotspot groups | 17 | 18 |
| plume candidates / retained | 137 / 7 | 33 / 3 |

Unchanged: benzene / toluene / TMB / xylene 99th percentiles (1.8 / 4.31 / 2.59 / 3.19 ppb),
median benzene 0.1 ppb, median HCN 1 ppb, La Casa scaling factors (1.15 / 1.23 / 1.38).

## Known exclusions

- `40_alert.R` (alerts add-on) reads `Suncor_alerts.csv` + orphan `lacasa_pbl.RData`; not part
  of any manuscript number → excluded.
- Figure-only scripts are run by `MAKE_FIGURES.R` after stage 1; they consume only pipeline
  outputs.

## Orphan artifacts (quarantined by CLEAN; produced by no script, consumed by no script)

`hs_df_{benzene,toluene,trimethylbenzene,xylene,h2s,hcn}.RData`, `res_h2s.csv`,
`lacasa_pbl.RData` — leftovers of pre-scripted interactive analyses. Nothing in the pipeline
writes them; they must not be cited for manuscript numbers.

## Fixes applied for reproducibility

- **Time convention** made explicit and asserted in 02, 06, P04 and H04; `36_hysplit` corrected
  from `America/Denver` to `MST` (35 of 60 HYSPLIT receptors had been launched an hour early);
  H04's `tz_local` argument removed entirely in favour of the constant `H04_TZ_LOCAL`.
- **Shared met helpers** (`plume_scripts/P00_met_helpers.R`) — one definition of the cloud
  preparation, the Pasquill classifier and the plume-geometry wind field, sourced by P05, P06,
  P09 and P10. Previously three copies had diverged, changing the stability class on ~6% of
  met rows.
- **P07** no longer admits events with no stability class; **P08** fails rather than defaulting
  missing plume geometry to the centreline, and marks ill-conditioned scenarios in the figures
  and in the all-scenarios CSV.
- **R07 / R99** report emission ranges from `tpy_metric`, screened to `usable == TRUE`.
- `R_scripts/18_...R` loads `mobile_corrected.RData` from the Suncor folder (was `~/Downloads/`
  root — stale-file hazard).
- `block_sf_risk` (script 20's input, formerly interactive) is constructed by `R04b`.
- Grids generated from scratch by `R00b_make_grids.R`. Cell boundaries shift vs the legacy QGIS
  grid, so segment ids and Fig 2 values change slightly.
- Methane: CR-only line endings handled; garage filter (100 m of ATOPs HQ) scripted; delays
  same as H2S (same Picarro).
