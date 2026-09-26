# Reproducibility manifest

**Scope note (2026-09-25).** `RUN_ALL_from_raw.R` runs `R00a` in its default mode, which
verifies the packet inventory, revisions and monthly-CSV coverage but does **not** re-derive
the monthly CSVs from the official XLSX packets. Those CSVs are therefore validated inputs
rather than primary ones on the default path. To close the loop back to the XLSX source, run
`DEEP=1` (content comparison) or `REBUILD=1` (regenerate the CSVs) - see `R00a` - which is the
reconstruction test this policy relies on.

**Policy (adopted 2026-08-15): every manuscript number must be reproducible from primary
inputs. No hand-made or interactive intermediate is accepted.**

Last full re-run: **2026-09-23/25**, from raw CDPHE inputs, after the CDPHE co-authors
required removal of every measurement within 300 m of the ATOPs headquarters. Every number
in the manuscript, the SI and the Shiny app comes from that run. Stage 1 (R01-R05) ran
2026-09-23; stages R06 onward were completed by `RUN_RESUME_from_R06.sh` (R06 133 min,
figures 901 min). 26 of 28 figure groups passed first time; group U was re-run after an
`apply` masking fix and group A after being killed on memory pressure. **Zero `[EDIT]`
flags** across every group log, i.e. nothing in the run disagrees with the documents.

The previous full re-run was **2026-08-21/22** (~9.2 h): it retained H2S across the 2023
inlet-contamination window (+93,992 values, +35 sampling days) and dropped the 28-30 May 2025
HCN calibration window (-27,739 values, -2 days).

Group J (Figure S3.1 route summary) failed in that run on a masked `shift()`; the call is now
namespaced as `data.table::shift` in `43_figure_s31_routes.R` (and in
`63_deheld_sensitivity.R`, where a masked lag would have been silent). **Re-run 2026-08-22,
passed in 21.2 min**, so `figureS31_runs_summary.csv` is current: 205 runs, matching the
audit's independent Site x day count.

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

To resume after stage 1 without redoing R01-R05 (the expensive part), use

```bash
bash ~/Downloads/Suncor/rerun_pipeline/RUN_RESUME_from_R06.sh
```

which pre-flights five R01-R05 outputs, runs R06 → R07 → methane → R99 → P09/P10 →
`MAKE_FIGURES.R`, never CLEANs, and keeps every stage under `caffeinate -i`.

**Read the per-group logs, not the exit code.** `MAKE_FIGURES.R` exits 0 even when a group
fails. Its `[PASS]`/`[FAIL]` lines go to `figures.log`, *not* to the per-group
`FIG_<g>_console.txt` files — a check that grepped the console logs (as both runners did
until 2026-09-25) never matched, so a run with failed groups still printed
"all groups completed" and "ALL STAGES COMPLETED". Both runners now read `figures.log` and
additionally warn when the number of groups started differs from the number of result lines.

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
SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/68_fetch_cdphe_inputs.R  # verifies inputs vs the CDPHE site
SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/69_cdphe_audit_mdls.R   # writes CDPHE_audit_MDLs.csv
SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/70_table_s31.R          # writes TABLE_S3.1.csv
SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/71_table_s11.R          # writes TABLE_S1.1_periods.csv
SUNCOR_BASE=~/Downloads/Suncor Rscript tests/audit_manuscript_claims.R
```

Run in that order: 69 reads the packets 68 verifies, 70 reads the MDL csv 69
writes, and the audit reads everything.

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
   ×1.165), on blocks with AirToxScreen benzene + population > 0. Implemented in R04b +
   script 20. **This replaces the manuscript's irreproducible Feb-2026 numbers** (1,120
   blocks / 2.4×): that aggregation was interactive, left no code, and no tested
   reconstruction (10 candidates) reproduces it.

   Reproducible result, 2026-09-23/25 run: **1,667 blocks, 126,527 residents**; AirToxScreen
   0.117–0.416 excess cases, mobile 0.108–0.383, **ratio 0.92**. Per block, mobile benzene is
   *lower* than AirToxScreen in **77%** of blocks (median ratio 0.78), ≥2× higher in 6%, and
   an order of magnitude higher in **3** blocks. Block-level correlation is nil (Pearson
   0.004, Spearman −0.040). Framing: the two datasets disagree in **spatial pattern**, not in
   overall level — screening models approximate the aggregate but misplace it spatially.

3. **Plume inversion**: WWTP-updated funnel, **37 candidate events → 4 retained**. Baseline
   intercepts 471 / 706 / 1,003 / 1,964 metric t/yr, **range 471–1,964**, mean 1,036.

   **No confidence interval is reported (decision of 2026-08-22).** Every one of the 112
   per-plume estimates across all 28 scenarios is positive (minimum 126 t/yr); the −8 lower
   bound previously quoted was an artifact of a symmetric t-interval on n = 4, where
   t(0.975, 3) = 3.182 and se = 328.1 give a margin of 1,044.2 against a mean of 1,036.0. A
   normal-theory interval is the wrong shape for a strictly positive, right-skewed quantity —
   21 of the 28 scenarios carried a negative lower bound for the same reason — and four
   intercepts do not support one. Both documents now report the range and the scenario means
   alone. For reference, a lognormal interval on the same four values gives a geometric mean
   of 900 t/yr and 343–2,358, which does not change the order-of-magnitude claim.

   Well-posed scenario means 420–1,687 metric
   t/yr over the 25 input perturbations, 420–2,223 over all 26; 2 of 28 scenarios (`wd+10`, `avg_1s`) are flagged `usable = FALSE` because the
   receptor lies beyond 2σy, where the inferred rate is governed by the Gaussian tail rather
   than by the measurement. The manuscript's "137 candidates / seven retained" traces to a
   pre-WWTP analysis vintage.

   **Emission column**: the metric-tons/year column is `tpy_metric`. `Q_ppm_m3_s` is a legacy
   volumetric quantity and `kg_s` is a rate — neither is t/yr. Use
   `pick_emission_col()` (in `diagnostics_helpers.R`); do not pattern-match column names.

4. **Hotspots**: **14** persistent multi-pollutant groups after the 300 m exclusion
   (group ids 4, 8, 10, 11, 12, 13, 22, 28, 29, 30, 34, 40, 43, 60), from 2,652 initial
   DBSCAN clusters → 217 persistent → 155 candidate → 37 / 14 / 8. The 2026-08 run had 17;
   the three that no longer qualify sat within the excluded radius or lost persistence with
   the records removed. Table S5.1 and its 56 panels (14 groups × 4) follow this set.

5. **300 m CDPHE-headquarters exclusion** (added 2026-09-23 at the request of the CDPHE
   co-authors): every measurement within 300 m of 39.785189, −105.104411 is removed. The rule
   lives in `03_checks_flags.R` section 3c, is unconditional, and carries a `stopifnot` that
   nothing inside the radius survives, so it holds across a CLEAN re-run. It removes **47,643
   of 2,602,928 records (1.8%) on 114 of the 203 sampling days**. The methane chain applies
   the same radius as its garage filter (M01).

6. **Concentration bases.** Three quantities recur and are not interchangeable. *Raw* =
   delay-corrected, native-cadence values as delivered, negatives and below-MDL retained;
   these are Table S3.1, the p99 event thresholds and the values quoted in the main text.
   *Background-corrected* = minus the rolling background (lowest 20th percentile over a
   20-minute window, SI S4.1.1); these underlie the 500 m maps, the census-block surface and
   the S7 hazard quotients. *Temporally scaled* = block-level background-corrected × the La
   Casa bin-weighted factors; used only where a 24-h average is required (the §3.3 benzene
   comparison, and the maximum sustained cell in Table S3.2). SI Table S3.2 reports the
   median and p99 on **both** bases, raw first and background-corrected in parentheses;
   `54_health_reference_table.R` emits both and filters the raw record exactly as
   `70_table_s31.R` does so its raw columns equal Table S3.1 cell for cell.

7. **Cadence**: native-cadence averaging (H2S 5 s, HCN 2 s, CH4 5 s), block mean assigned only
   to seconds that already held a value, applied after delay correction. Plume detection is
   exempt and uses the delivered `*_raw` signal.

## What the 2026-08-21/22 re-run changed

| item | as submitted | this run |
|---|---|---|
| 1-s measurements | 2,708,051 | 2,602,928 |
| 99th pct H2S / HCN | 5 / 12 ppb | 4.6 / 11 ppb |
| median H2S | 0 ppb | 0.2 ppb |
| census blocks ≥1 point | 2,832 | 2,857 |
| common blocks / residents | 1,120 / 83,828 | 1,668 / 126,607 |
| mobile : AirToxScreen risk ratio | 2.4 | 0.92 |
| persistent hotspot groups | 17 | 17 (different membership) |
| plume candidates / retained | 137 / 7 | 37 / 4 |

Unchanged: benzene / toluene / TMB / xylene 99th percentiles (1.8 / 4.31 / 2.59 / 3.19 ppb),
median benzene 0.1 ppb, median HCN 1 ppb, La Casa scaling factors (1.15 / 1.23 / 1.38).

## What the 2026-09-23/25 re-run changed

| item | 2026-08-21/22 | this run |
|---|---|---|
| 1-s measurements | 2,602,928 | 2,555,285 |
| common blocks / residents | 1,668 / 126,607 | **1,667 / 126,527** |
| persistent hotspot groups | 17 | **14** |
| La Casa scaling (benzene/toluene/xylene) | 1.149 / 1.228 / 1.377 | **1.165 / 1.274 / 1.443** |
| p99 toluene / TMB / xylene | 4.31 / 2.59 / 3.19 | **3.80 / 2.14 / 2.83** |
| p99 H2S / HCN | 4.6 / 11 | **4.8 / 11** |
| mobile benzene risk | 0.108–0.384 | 0.108–0.383 |
| TRI facilities quoted in §2.1 | 752 (facility-*year* rows, statewide) | **67** in the route bounding box |

Unchanged and re-verified: the plume funnel (**37 candidates → 4 retained**), the four
baseline intercepts (471 / 706 / 1,003 / 1,964 t/yr, mean 1,036.04), the well-posed scenario
envelope (126–3,929 t/yr over 104 of 112 rows), the risk ratio 0.92, and every S7 organ
hazard index (endocrine 1.61 / 8.71, respiratory 0.371 / 4.97, neurological 0.031 / 0.555,
hematological 0.015 / 0.237).

**Two emission ranges exist and must not be confused**: the *baseline per-plume* range
471–1,964 t/yr (mean 1,036) is what both documents quote; the *all-well-posed-scenario*
envelope 126–3,929 t/yr is a `REF` diagnostic and is deliberately not quoted.

## Known exclusions

- **Within 300 m of the CDPHE ATOPs headquarters** (39.785189, −105.104411) — see canonical
  definition 5. 47,643 records on 114 days.
- `40_alert.R` (alerts add-on) reads `Suncor_alerts.csv` + orphan `lacasa_pbl.RData`; not part
  of any manuscript number → excluded.
- Figure-only scripts are run by `MAKE_FIGURES.R` after stage 1; they consume only pipeline
  outputs.

## Orphan artifacts (quarantined by CLEAN; produced by no script, consumed by no script)

`hs_df_{benzene,toluene,trimethylbenzene,xylene,h2s,hcn}.RData`, `res_h2s.csv`,
`lacasa_pbl.RData` — leftovers of pre-scripted interactive analyses. Nothing in the pipeline
writes them; they must not be cited for manuscript numbers.

`hotspot_source_fingerprint_outputs/hotspot_ratio_clusters.csv` (2026-03-18) is a stale orphan:
no script in the current pipeline writes or reads it, and it predates the delay correction.

## Numeric-claim audit (2026-08-21)

`tests/audit_manuscript_claims.R` recomputes, from `mobile_wswd.RData`, every
number the text asserts about the measurement record, so a future re-run either
reproduces the printed claim or shows what moved. It currently passes on all
checks. Verified this way and now stated correctly in both documents:

| claim | value |
|---|---|
| 1-s measurements | 2,555,285 |
| runs (Site x day) / unique sampling days | 205 / **203** (2023-02-16 to 2025-06-23) |
| weekday split | 8 Mon, 39 Tue, 48 Wed, 50 Thu, 58 Fri |
| measurements 9 am - 2 pm | 88.6% |
| hourly fractions, 7-8 am through 4-5 pm | 0.1, 2.8, 12.2, 19.3, 21.1, 20.6, 15.4, 6.2, 1.7, 0.6 % |
| Suncor route r: tol-xyl / TMB-xyl / benz-TMB | 0.94 / 0.90 / 0.70 |
| Holly route r: tol-xyl / TMB-xyl / tol-TMB | 0.84 / 0.88 / 0.75 |
| max r, H2S+HCN vs the aromatics | 0.103 (Holly), 0.164 (Suncor) |
| hotspot max exceedance-days, across the 14 groups | 15 to 69 (group 4, toluene) |
| most persistent group | group 4 - toluene on 61 days, 179 pollutant-days across four pollutants |
| most heavily sampled group | group 9 - 37,669 measurements within 100 m |
| retained plumes | 4, at 1.95 / 3.55 / 4.11 / 4.30 km, Nov 2023 / Jan 2024 / Apr 2023 / May 2024 |
| route runs (Figure S3.1) | 205 - 101 Holly, 104 Suncor/P66; median run 4.7 / 4.3 h |
| GPS segment speed | median **26.0** km/h, IQR 13.8-38.4, 95th 73.8; 13% stationary; only 32% inside the submitted "30-60 km/h" |
| exceedance-days, metric A | MASTER `max_n_days`, what Figure 4A scales circles by: **15** (g70) to **66** (g3) |
| exceedance-days, metric B | max `n_days_high` within 100 m vs campaign p99, what Table S5.1 prints: **5** (g23) to **53** (g4, toluene) |
| distinct exceedance days | union of `days_high` across pollutants: **8** (g23) to **60** (g4); group 9 = 42 |
| S1.4 QA/QC counts | MD at half MDL **1.0-9.0%**; benzene at -0.10 ppb **191,962**; BR with a value **0**; AL/BH **0** benzene, **480-8,459** others; negatives surviving **358,048** benzene / **627,134** H2S |
| well-posed scenario means | **420 to 2,223** t/yr over all 26; 420 to 1,687 over the 25 that perturb an input rather than dropping the reflection term |

Errors this pass found and corrected, none of which the re-run introduced:

- Results 3.1 said "~2 million measurements over **230** unique days" - a
  transposition of 203, which the Methods section had right.
- The SI's hour-of-day bins were labelled **an hour early**. The listed fractions
  belong to 7-8 am onward; as labelled they summed to 82% for 9 am - 2 pm while
  the same paragraph claimed 88%.
- The submitted "maximum exceedance-days ranged from 4 to 53" is not a metric the
  run produces. `MASTER_hotspot_group_index.csv` gives **15 to 66** for `max_n_days`,
  which is what Figure 4A scales its circles by. An earlier pass in this file
  asserted that no metric produces 179 either; that was wrong - 179 is group 4's
  `total_n_days`, the sum of its per-pollutant exceedance days, and the manuscript
  now says so explicitly rather than calling them distinct days. Group membership
  was re-derived from the `cent_out_*_persistent.csv` centroids and reconciles with
  the master index on `total_n_days`, `max_n_days` and `total_measurements` for
  17 of 17 groups.
- Two sentences in S6.5 still spoke of "the 7 plumes".
- Both documents reported the well-posed scenario range as 415-1,774 while also
  saying 26 scenarios are well-posed; 1,774 is the maximum over 25 of them.
- Both S2 instrument paragraphs pointed readers to Table S3.1 (descriptive
  statistics) for instrument specifications, which are in Table S1.1.
- The main text cited "Figure S1.1" for the route maps; there is no Figure S1.1.
- **Figure 1 was never swapped in.** `42_figure1_sampling_density.R` rebuilds it
  (redesigned per Reviewer 1: side-by-side 500 m sampling-density panels) but sat in no
  MAKE_FIGURES group, so the re-run never regenerated it and the manuscript still carried
  the submitted 2048x1448 image with a caption describing the old route map. Group **J3**
  added; figure and caption swapped 2026-08-22. The figure is content-current -
  `figure1_cell_counts_by_route.csv` sums to 2,555,285 over 924 cells, exactly this run's
  1-s record count (it read 2,602,928 until the 300 m exclusion; the claim held then and
  holds now, only the total moved).
- S1.4 said AL/BH discard "between 480 and 8,459 rows per pollutant". True for five
  pollutants; **benzene has none at all**. The paragraph's other three claims verified
  exactly (see below).
- S4.1.2 justified the 500 m grid with "the fast road speeds of the mobile
  laboratories (30-60 km/h)". Over the 2,596,958 GPS segments behind Figure S3.1
  the median is **26 km/h** (IQR 14-38, 95th 74) and only 32% of segments fall in
  30-60; 13% are stationary. Reworded to the measured distribution plus the
  sampling argument that actually supports the cell size - at the 95th percentile
  a 1-s sample advances 21 m and a 500 m cell takes ~24 s to cross.

## Inputs are fetched from the CDPHE site, not assumed

`R_scripts/68_fetch_cdphe_inputs.R` downloads the 20 quarterly packets and the
ten HB21-1189 read-me documents from
`colorado.gov/airquality/air_toxics_repo.aspx` and compares each against the
local copy by MD5. It **verifies by default** and only writes when
`SUNCOR_FETCH=1`, so a re-run cannot silently swap an input underneath the
analysis. Revisions are pinned to the ones this study used - `_r3` for 2023 Q1
through 2024 Q2, `_r2` for 2024 Q3 through 2025 Q2 - because CDPHE re-posts
packets under new suffixes and a bare filename would not be reproducible. It
writes `CDPHE_download_manifest.csv`.

With 68 in place the chain runs public URL -> packets -> audit MDLs -> derived
csv -> table -> document, with no hand-entered link. The parts that still are
not public: the methane deployment CSVs (CDPHE direct), HRRR met (fetched live
from NOAA), and the census/EPA layers (fetched live from their own sources).

## QA/QC qualifiers: what the null rule actually removes

Measured across all 58 monthly CSVs on 2026-08-21:

| code | rows flagged | rows flagged **and carrying a value** |
|---|---|---|
| BR (Sample value below acceptable range) | 32,158 - 471,975 per pollutant | **0** |
| AL (Voided by Operator) | - | 480 - 8,459 |
| BH (Interference/co-elution) | - | 1 - 2 |

CDPHE blanks the value itself whenever it sets BR, so voiding BR removes
nothing. The concern that it might conflict with this study's policy of keeping
negative values does not arise: **358,048 negative benzene values and 627,134
negative H2S values survive** into the record. The only values the null rule
actually removes are AL and BH - explicit operator voids and identified
interferences - a few thousand rows per pollutant.

All ten Informational Only codes (CD, CG, IH, IL, IR, IT, QG, QP, QT, QW) and all
five Quality Assurance codes (EH, LJ, MD, NS, QX) are kept, MD - value below MDL -
among them. `03_checks_flags.R` now asserts that no BR-flagged row carries a
value, so if a future CDPHE revision changes that, the pipeline stops rather than
quietly discarding data.

## Every exclusion window now cites CDPHE

All four windows in `03_checks_flags.R` are CDPHE's own findings, quoted in the
code. None of them was our judgement call.

**16 April - 20 September 2023, all six pollutants - inlet line contamination.**
Q2 and Q3 2023 read-me: *"A previously unknown issue of inlet line contamination
was discovered on September 20, 2023 after reviewing quarterly data and a
physical inspection of the inlet lines which confirmed the presence of visible
residue... suspected to have started in the second quarter of sampling on
approximately April 26, 2023, based on elevated daily average concentrations of
the target compounds, resulting in elevated quarterly averages for compounds
detected via PTR-ToF-MS and CI-ToF-MS (benzene, toluene, xylene,
trimethylbenzene, and hydrogen cyanide)... Currently, this is not corrected for
in the data... sampling inlet lines were replaced on September 20, 2023 and
monthly averages of target compounds decreased."*

The record agrees. Median before the window vs inside it: benzene 0.10 -> 0.30
(3.0x), toluene 0.38 -> 0.75 (2.0x), xylene 0.52 -> 0.91 (1.8x), HCN 1.00 ->
2.00 (2.0x). Monthly medians drop straight back after the lines were replaced -
benzene 0.90 in July, 0.80 in August, 0.30 in September, **0.10 in October**.

Two deliberate departures from CDPHE's wording, both now stated in the SI:

1. The window starts **16 April**, ten days before CDPHE's "approximately April
   26". Their onset is inferred from elevated daily averages, not observed, so
   the earlier start is the conservative reading. Both documents say so.
2. **H2S is RETAINED across the window** (`EXCLUDE_H2S_2023 <- FALSE`, changed
   2026-08-21). CDPHE's affected list is the PTR-ToF-MS and CI-ToF-MS compounds;
   H2S comes from the Picarro CRDS on its own inlet and is not named, and it
   shows no elevation in the window at all - median 1.00 ppb both before and
   during, ratio 1.00x, against 3.0x for benzene, 2.0x toluene, 1.8x xylene and
   2.0x HCN. Retaining it recovers **104,668 values, 6% of the H2S record**. Set
   the switch TRUE to restore the earlier, more conservative treatment.

**13-14 August 2024, the four aromatics - Eiger baseline correction off.**
Q3 2024 read-me: the Vocus Eiger's *"automatic instrument baseline correction was
not operational"* on those deployments, *"resulting in ambient concentrations
being reported as artificially elevated since the instrument baseline signal was
not accounted for"*. The Eiger measures benzene, toluene, xylenes and
trimethylbenzene, which is exactly the set the code excludes.

**On or before 22 January 2025, HCN - background-corrected, not absolute.**
Q1 2025 read-me: HCN before that date *"received empirical background
correction"*; from that date the measurements *"represent absolute
measurements"*.

**28-30 May 2025, HCN - bad sensitivity calibration.** Q2 2025 read-me: *"Due to
an inaccurate sensitivity calibration performed the week of May 27, 2025"*, those
three days rest on averaged calibrations. Removing them drops 29,085 values (28
and 30 May; no sampling on the 29th).

The 2-3 January 2025 HCN window is subsumed by the 22 January cutoff.

### The caveats live only in the PDFs

Searched every quarterly packet: the workbooks carry the MDL table, the quarterly
and monthly statistics, and the qualifier-flag legend, and **no data-quality
notes at all**. Every advisory above exists only in the HB21-1189 read-me PDFs.
That is why `68_fetch_cdphe_inputs.R` downloads and hashes the read-mes alongside
the packets: without them the exclusions cannot be justified from the archive.

### Two further caveats from the read-mes, checked

- **"data within +/- the value of the MDL are flagged 'MD' and replaced with
  0.5*MDL"** (Q1 2023 read-me). This does **not** describe the mobile 1-second
  files. Of the MD-flagged values that carry a number, only 1-9% sit at half the
  audit MDL, and the rest form a continuous distribution including negatives
  (191,962 benzene values at -0.1 alone). The delivered mobile values below the
  MDL are measurements, not substitutions, so this study's policy of keeping them
  keeps real data.
- **Benzene was reprocessed on 10 December 2025** with high-resolution analysis
  to separate benzene from interfering species. The local packets postdate that,
  and `68_fetch_cdphe_inputs.R` will confirm they match what is posted now.

## What the documents now say about QA/QC and processing

Both documents previously left most of this implicit. As of 2026-08-21:

**Manuscript, Methods 2.1.1** carries a new paragraph giving the single QA/QC
rule, the four exclusion windows with CDPHE as the authority for each, the H2S
decision, and the delay correction and native-cadence averaging in outline,
pointing to S1.4 for detail.

**SI S1.4** was rewritten:

- the flag list was missing five null codes (BK, BR, EC, MB, XX) and is now
  complete, with the informational and quality-assurance codes named as retained;
- it now records that BR never accompanies a value, so the only codes that
  actually discard a number are AL and BH;
- the inlet-contamination paragraph gives CDPHE's account, our 16 April start and
  the reason for it, and the H2S retention with the evidence;
- the August 2024 paragraph now states the consequence, not just the fault, and
  that only the Eiger's four aromatics were removed;
- the 28-30 May 2025 HCN window is described for the first time;
- three new paragraphs describe the delay correction and native-cadence
  averaging, including why the averaging block is anchored on the instrument's
  delivery clock rather than the shifted timestamp.

### The read-me / delivered-file discrepancy, now stated

SI S1.4 describes CDPHE replacing sub-MDL values with 0.5 x MDL and negatives
with zero. **That is not what the one-second files contain.** Of the MD-flagged
values carrying a number, only 1-9% sit at half the audit MDL; the rest form a
continuous distribution including 191,962 benzene readings at -0.1 ppb. The rule
describes CDPHE's summary products, not the mobile record. The SI now says this
explicitly, because a reviewer reading the read-me would otherwise conclude that
the values this study retains are imputed constants when they are measurements.


## Table S1.1 dates come from the measurement files

`R_scripts/71_table_s11.R` derives each instrument's measurement period from the
files it produced and writes `TABLE_S1.1_periods.csv`. Against the data, the
submitted row was wrong in three of three cells:

| column | submitted | measured |
|---|---|---|
| mobile (Vocus Eiger) | Feb 26 2023 - Mar 13 2025 | **Feb 16 2023 - Jun 23 2025** |
| Vocus 2R, La Casa | Jun 26 - Aug 2 2024 | **Jun 24 - Aug 2 2024** |
| Vocus Elf, La Casa summer | Jun 2 - **Sep** 8 2023 | Jun 2 - **Aug** 8 2023 |
| Vocus Elf, La Casa winter | Dec 20 2023 - Feb 20 2024 | same |

The script determines the date order (day-first vs month-first) from the data
rather than assuming it - the two La Casa families differ - and rejects a
two-digit year silently parsed as year 23 AD.


## Detection limits come from CDPHE, not from us

The HB21-1189 read-me PDFs do not print MDL values. They say: *"The reported
Method Detection Limits (MDLs) for each compound and asset are reported on the
Quarterly Summary tab of the Data Summary."* `R_scripts/69_cdphe_audit_mdls.R`
reads that tab out of all 20 quarterly packets and writes
`CDPHE_audit_MDLs.csv`; `70_table_s31.R` uses it. Both routes carry the same MDL
table in every quarter, which the script asserts rather than assumes.

Checked against that source on 2026-08-21, SI Table S1.2 had four defects:

| what the SI printed | what the packets say |
|---|---|
| EMU hydrogen cyanide, Jan-Mar 2025: **0.18** | **18.0** ppbV |
| EMU hydrogen cyanide before 2025: **blank** | **5.0** ppbV from 2023 Q4 |
| nothing for **2025 Q2** | CAT benzene 0.3, HCN 10.0; EMU benzene 0.5, HCN 2.0 |
| EMU trimethylbenzene 0.44 ends **Sep 2024** | runs through **Dec 2024** |

Everything else matched exactly. The misplaced decimal in the first row is what
made the below-MDL fraction for HCN look wrong: with 0.18 ppbV it computes to
53%, with the published 18.0 ppbV it is **96.4%** - which is what the submitted
table said. The row was right; the transcription of the limits was not.

With the packet values the below-MDL fractions are benzene **93.1%**, toluene
39.0%, xylene 55.5%, trimethylbenzene 76.1%, H2S **97.5%**, HCN **96.4%**, and no
measurement is left without a published limit. Table S1.2 is now regenerated from
the same csv by `manuscript_build/rebuild_table_s12.py`.

### The HCN cutoff has a published reason

`03_checks_flags.R` drops all HCN on or before 22 January 2025. CDPHE's
HB21-1189 read-me for 2025 Q1 explains it: *"HCN data prior to January 22, 2025
received empirical background correction. Data from January 22, 2025 to present
represent absolute measurements."* The cutoff keeps only the absolute
measurements. That reason is now stated in the SI and in the manuscript's
Limitations section; it was previously applied with no comment anywhere.

The other two exclusion windows - 16 April to 20 September 2023 for all six
pollutants, and 13-14 August 2024 for the aromatics - still carry **no published
or in-code justification**. They need one before submission.

### Dates still to confirm

Table S1.1 gives the mobile Vocus Eiger's measurement period as **26 February
2023 - 13 March 2025**. The mobile aromatics record in this study runs
**16 February 2023 - 23 June 2025**, and both vans report aromatics after 13
March 2025 (CAT on 7 days, EMU on 17). Either the row means something narrower
than "the period this instrument contributed data to this study", or the dates
are wrong. The quarterly packets name the aromatics instrument only generically
("TofWerk: Proton Transfer Reaction Time-of-Flight Mass Spectrometer"), so this
cannot be settled from the public source.

Also worth a look: `03_checks_flags.R` classes **BR** as a null qualifier and
voids it, while the CDPHE read-me defines BR as *"Negative values (less than
-MDL for benzene/H2S; any negative values for other compounds)"*. Voiding BR
therefore removes some negative values, which sits awkwardly beside the stated
policy of keeping them. Negative values do survive in the record (benzene
minimum -1.40 ppb, H2S -5.00 ppb), so the effect is partial, but the rule and
the policy should be reconciled explicitly.


### Table S3.1 is generated, not typed

`R_scripts/70_table_s31.R` builds **every cell** of Table S3.1 from the 58 monthly
CDPHE CSVs plus `mobile_wswd.RData` and writes `TABLE_S3.1.csv`; the document
table is built from that CSV by `manuscript_build/rebuild_table_s31.py`. It is
wired into `MAKE_FIGURES.R` as group **J2**, so a re-run refreshes it.

```bash
SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/70_table_s31.R
```

The rows now name the filters the code actually applies, in order. The old label
"No of measurements after removing additional contaminated data points" described
a step that appears nowhere in the source; it is in fact four hard-coded date
windows in `03_checks_flags.R` (lines 107-130), reproduced verbatim in the
generator and listed in the table caption:

| exclusion | applies to |
|---|---|
| 2023-04-16 .. 2023-09-20 | all six pollutants |
| 2024-08-13, 2024-08-14 | the four aromatics |
| 2025-01-02, 2025-01-03, and everything on or before 2025-01-22 | HCN |

**Sourced (2026-09-17/22).** All four exclusion windows are now attributed to CDPHE
and documented in the source; see `claude/exclusion_windows_sourced.md` and the
comments in `03_checks_flags.R`. The earlier note here - that none of them carried a
comment and the reasons still needed writing down - is superseded.

The chain, per pollutant (benzene / toluene / xylene / TMB / H2S / HCN):

| stage | counts |
|---|---|
| reported by CDPHE | 2,076,250 / 2,156,354 / 2,120,529 / 2,039,081 / 1,843,073 / 2,010,071 |
| retained after QA/QC | 2,076,250 / 2,153,823 / 2,117,999 / 2,036,549 / 1,834,614 / 2,009,591 |
| with valid GPS | 2,056,331 / 2,076,909 / 2,041,754 / 1,965,175 / 1,743,052 / 1,924,362 |
| after the date exclusions | 1,794,695 / 1,787,960 / 1,752,894 / 1,676,498 / 1,638,384 / 554,184 |
| in the analysis set | 1,553,754 / 1,551,628 / 1,521,730 / 1,461,849 / 1,376,977 / 509,210 |
| sampling days | 165 / 164 / 164 / 159 / 164 / **41** |
| below the audit MDL | 95.2% / 39.0% / 55.5% / 76.1% / 97.5% / 52.9% |

Every percentile, the maximum and the mean reproduce the submitted values
exactly; the table now prints them all to two decimal places, which is why some
cells look different without having changed (HCN's 75th percentile was printed as
"2" and is 1.50).

**HCN coverage.** The `<= 2025-01-22` exclusion means HCN rests on **41 sampling
days between 22 January and 23 June 2025**, not the 203-route-day campaign. Both
documents now say so, and the Limitations section carries the caveat.

**The below-MDL row** uses the audit MDLs in Table S1.2 applied by lab and by
period, with the last listed period carried forward past March 2025 (the table
stops there; sampling runs to June). Two things about that basis are worth
confirming rather than assuming:

1. The EMU HCN audit MDL is listed as **0.18 ppbV** - below the 0.5 ppb grid the
   delivered HCN values are reported on, and 28x smaller than CAT's 5 ppbV for the
   same period. It is the single reason the pooled HCN figure is 52.9% (CAT alone
   gives 88.7%, EMU alone 40.8%). The generator prints the per-lab split so the
   driver is visible.
2. Table S1.2 lists a CAT HCN MDL from January 2023, but no HCN survives the
   pipeline before 22 January 2025.

The old row (93 / 40 / 56 / 77 / 95 / 96 %) was also inconsistent with the
percentile rows beside it: H2S's 99th percentile is 4.6 ppb against a CAT MDL of
5-6 ppb, so more than 95% of H2S values must be below MDL. The generated row says
97.5%.

## SI section numbering

The SI had **two** Heading-1 sections numbered S5. They are now one:

```
S5 Hotspots
  S5.1 Hotspot Sensitivity Analysis
    S5.1.1 Methods
    S5.1.2 Results
  S5.2 Multipollutant Hotspot Characteristics
```

Figure and table numbers are unchanged: Figures S5.1-S5.5 and Table S5.1 were
already one continuous sequence across the two former sections. S2's subheadings,
which were numbered 2.2.1 / 2.2.2, are now S2.1 / S2.2.

**Added 2026-09-25: S7.4 "Sensitivity to temporal scaling"**, carrying Tables S7.3-S7.7 and
Figures S7.1-S7.2. La Casa measures only benzene, toluene and C8 aromatics, so no 24-h
scaling factor exists for 1,2,4-TMB, H2S or HCN — and H2S and HCN drive every hazard index
in S7. `77_health_scaling_sensitivity.R` recomputes the organ indices under four scaling
constructions (exact arithmetic: HQ is linear in concentration and each scaling is one
multiplicative constant, so no pipeline stage is re-run) and reports the break-even factor at
which each index would cross 1. `78_diurnal_scaling_evidence.R` estimates the hour effect
**within 500 m cells** — hour of day is confounded with location on a fixed route — and shows
the La Casa night:day ratio ranks the three measured species exactly as their 24-h factors
rank while within-window shape does not, so the factor is an overnight quantity and the
missing H2S/HCN factor is a measurement gap rather than an analysis choice.

**Added 2026-09-25 (later): the scaling toggle on Shiny page 7.** Page 7 previously
showed a single hazard basis and said nothing about scaling, while its 500 m map sat on a
*different* basis from its own sidebar: `73_cumulative_risk.R` writes EC/HQ columns that
already carry the La Casa factor for benzene, toluene and xylenes (a `scale_factor` column
in `TABLE_cumulative_HQ_by_cell.csv`), whereas `74_health_hazard_screening.R` writes
Table S7.1 unscaled. The app read both straight and labelled neither. Three changes close
this:

- `77_health_scaling_sensitivity.R` now also writes
  **`TABLE_S7.3b_scaling_by_pollutant.csv`** — the S7.3 scenarios kept per pollutant, with
  the factor applied and its provenance (`La Casa, measured` / `borrowed: …` / `unscaled`)
  as columns. It asserts that this table sums by organ to Table S7.3.
- `shiny_app/prep_app_data.R` reads S7.3, S7.3b and S7.4 into `hazard.rds`, and rebuilds the
  cell surface from **one** baseline: the unscaled HQ recovered as `mean_ppb / rfc`, then
  multiplied by the scenario factor. It checks that `unscaled × scale_factor` reproduces
  73's own `HQ_mean` (rel. diff ≈ 1e-14) before using it, and fails loudly on any species or
  target-organ name it cannot map between the two files.
- `shiny_app/app.R` exposes scenarios A–D as a radio control. The organ-index table, the
  per-pollutant chronic table (now carrying factor and basis columns) and the map all follow
  it; the acute screen does not, and says so — a 24-h-equivalence factor adjusts a long-term
  mean, not a short-term peak. The break-even table (S7.4) is shown alongside, and the
  sidebar text naming the measured factor range and the break-even factors is built at load
  from the written tables rather than typed in.

Two display defects were fixed in the same pass. The page 7 map was continuous
`log10(HI)` on **reversed** magma, which put the darkest colour on the *lowest* cells and
made the legend read upside down; it is now fixed HI bins on the same light→dark ramp as
page 1, with a break exactly at the screening benchmark (`right = FALSE`, so HI = 1 lands in
the "at or above" bin) and a `-Inf` floor so the negative-HI cells that come from retained
below-background values still draw. And CARTO began watermarking its keyless raster
basemap tiles with "API KEY REQUIRED", which was printing diagonally across every map in
the deployed app; `base_map()` now uses `Esri.WorldGrayCanvas`, with a fallback to
`CartoDB.Positron` on any leaflet build that lacks the entry.

Re-run order after touching any of this: `77_health_scaling_sensitivity.R`, then
`shiny_app/prep_app_data.R` (which syncs `.rds` into the repo copy), then push — Posit
Connect Cloud redeploys on push. Do **not** run `writeManifest()`.

**Added 2026-09-26: S4.6 "Sensitivity to the background definition"**, carrying Table S4.1
and Figure S4.12. The local background of S4.1.1 rests on two constants taken from the
literature rather than estimated from these data: the percentile (20th) and the rolling
window (20 minutes). `79_background_sensitivity.R` re-runs the whole background chain of
scripts 10-11 under a 3x3 grid - 10th/20th/30th percentile x 10/20/30-minute window - and
propagates all nine arms to the 500 m cell surface, the census-block surface, the benzene
risk comparison (script 20) and the S7 block-resolved hazard indices (script 74). Nothing
is rescaled after the fact: each arm recomputes the rolling percentile, the run median and
both correction branches from `mobile_wswd.RData`.

Two things make this trustworthy rather than merely plausible:

- **The engine is gated against the pipeline itself.** Before any sensitivity arm is
  reported the script re-runs the published setting and requires it to reproduce every
  `baseline_*`, `median_bg*` and `s*` column of `mobile_corrected.RData` **exactly** -
  2,555,285 rows x 6 pollutants, maximum absolute difference 0, zero NA-pattern
  mismatches. It `stop()`s otherwise. (The reimplementation exists because the pipeline's
  `slider::slide_index_dbl` path costs ~19 min per arm; the Rcpp Fenwick-tree engine in
  the script costs ~3 s and is bit-identical, including R's type-7 quantile interpolation
  `(1-h)*x[lo] + h*x[hi]` - writing it as `x[lo] + h*(x[hi]-x[lo])` differs at 1e-16 and
  fails the gate. A pure-R `slider` fallback runs automatically if Rcpp cannot compile.)
- **The point-in-block join is done once and reused by all nine arms.** It reproduces the
  published block surface to 0.0012% of observations (23 of 1,891,525 boundary
  assignments differ, because script 18 joined against the full Colorado block layer in
  geographic coordinates while this script joins against the saved overlap layer); the
  published population-weighted block benzene concentration is reproduced to 0.006%.
  Because the same assignment is used in every arm, this residual cancels in every
  arm-to-arm comparison reported in S4.6.

Not re-run, by design: the hotspot chain (its detector thresholds the RAW `*_ppb` columns
against a campaign 99th percentile and never reads a background-corrected column, so it is
algebraically independent of this grid) and the Gaussian plume inversion (frozen for this
revision).

Outputs: `TABLE_S4.1_background_sensitivity.csv`,
`TABLE_S4.1b_background_sensitivity_cells.csv`,
`FinalFig/FIG_S4.12_background_sensitivity.png`. Registered in `MAKE_FIGURES.R` group X,
last in the list - it must run after the main pipeline has written
`mobile_corrected.RData`, since the gate reads it. ~6 min with Rcpp. The script self-checks
all 24 numbers quoted in SI S4.6 with `[OK]`/`[EDIT]` lines, like 72 and 74 do for their
sections.

**Added 2026-09-26: `tests/audit_si_prose.R`.** Every other harness here checks the RUN against
a hardcoded claim string. Nothing checked the DOCUMENT against the run, and that gap let two
rounding slips reach the SI: section S4.6 read "0.154" where the source is 0.1534957 (rounds to
0.153) and "0.107" where the source is 0.1064980 (rounds to 0.106) - the second contradicting
Table S4.1 in the same section. Script 79's own `[OK]` lines passed throughout, because its claims
carry the source values at four decimals and the slip was made transcribing them to three.

The new test reads the `.docx` directly, re-derives each quoted number from the CSV the pipeline
wrote, rounds HALF-UP to the precision the prose uses (R's `round()` is banker's rounding, which is
not how a person rounds), and requires the sentence fragment to appear verbatim. It covers section
S4.6, section S6.5.2 and the two manuscript passages that quote them - 20 checks. Because the
`.docx` files are deliberately outside the repository, a missing document is a SKIP rather than a
failure; override the locations with `SI_DOCX` / `MS_DOCX`.

    SUNCOR_BASE=~/Downloads/Suncor Rscript tests/audit_si_prose.R

Also on 2026-09-26: `74_health_hazard_screening.R` carried `0.374` as the respiratory
population-weighted HI "that S7 says", while the SI has said `0.371` since the HQ300 re-run. The
run gives 0.371, so the check passed only on its 1% tolerance. Corrected to 0.371, so the claim is
again a faithful record of the document.

## Manuscript and SI figure provenance (2026-09-25)

Every numbered figure in both documents is the file the 2026-09-23/25 run wrote; none is
hand-edited or carried over. Re-embedded 2026-09-25: **44 SI figures, 4 manuscript figures
and the 56 Table S5.1 panels** (14 groups × 4), each verified afterwards against its current
source by alpha-composited grayscale comparison (all RMS < 0.7 against a noise floor of
~0.5). Before that pass, 35 of 44 SI figures and 3 of 4 manuscript figures were stale: the
documents had been built on 23 Sep at 16:10, *before* that day's run finished at 17:33, and
the 23 Sep vet covered text and numbers only.

Three traps in this operation, all of which produced silent wrong answers first:

1. **The part names in `swap_si_figs.py` are dead.** The SI has been rebuilt since; every
   `word/media/imageNN` number changed. Derive `rId → part` from
   `word/_rels/document.xml.rels` and match figures to parts by *content*, then reverse-look
   up the rId, asserting exactly one embedding rId per part.
2. **Caption proximity mis-assigns figures.** A "next `Figure SX.Y` within N characters"
   heuristic shifted S5.6-S5.9 by one, because an unlabelled image sits in that block.
3. **A `<w:drawing>` must be inside a `<w:r>`.** Placed directly in `<w:p>` it validates
   perfectly — well-formed XML, declared rIds, present media, unique docPr — and renders as
   *nothing*. Only a PDF proof catches it.

Older provenance notes, from the 2026-08-21 pass, follow. The swap scripts (`swap_figs.py` for the manuscript,
`swap_si_figs.py` for the SI) rewrite the `word/media` part in place and recompute
`<wp:extent>`/`<a:ext>` from the new image's aspect ratio, so nothing is stretched.

| document | figures replaced | not replaced |
|---|---|---|
| main | Figures 2, 3, 4 | Figure 1 (route map; no pipeline script regenerates it) |
| SI | Figures S3.1–S3.10, S4.1–S4.8, S5.1–S5.5, S6.1–S6.5 (28) | Equation S1 graphic |

Refitting caught four figures that the submitted SI displayed at the wrong aspect ratio
or wider than the 6.5 in text column: S3.9, S3.10, S5.1 and S5.3 (plus S4.2, S4.5 and
S4.6, laid out 8.5 in wide and running into both margins).

**Table S5.1** is regenerated from `hotspot_group_reports/`: 18 rows, one per persistent
group in `MASTER_hotspot_group_index.csv`, each naming its group id and carrying all four
panels its own caption promises (local 100 m map, regional map, wind-coloured pairwise
scatterplots, NWR polar plots). Exceedance-day counts come from
`group_<id>_pollutant_highday_table.csv`; the nearest TRI facility comes from
`MASTER_hotspot_group_index_with_TRI.csv`. The submitted table had 17 unlabelled rows and
only three of the four panel types.

**Figure S6.2's caption** said the panels were "the last 7 plumes in time"; the run retains
3 (ids 6, 9, 28) and the figure shows three facets. Corrected.

## Fixes applied for reproducibility

### Found during the 2026-09 re-run
- **`P04_join_with_mobile_toxics_data.R`** — `plan(multicore)` forks, and forking a process
  that holds a live multi-threaded Python interpreter (reticulate/Herbie) deadlocks. R06 hung
  for 6 h 15 min with no output. Now `plan(sequential)` unless `HRRR_PARALLEL` is set.
- **`P03_code_to_download_hrrr.R`** — the nearest-grid-cell search scanned all 1,905,141 HRRR
  cells once per point per cube (~62 h projected for R06). Replaced with a cropped batch
  search: verified identical indices for 400 query points on a synthetic rotated HRRR-sized
  grid, 233× faster. R06 now runs in 133 min.
- **`M04_sourceprob_map.R`** — `apply()` is masked by the `raster`/`terra` S4 generic, which
  has no method for a plain matrix, whenever the methane chain runs inside `MAKE_FIGURES`
  group U. The script loads only data.table and ggplot2, so it passed standalone and failed
  only in that calling context. Now `base::apply`.
- **`23_tri.R`** — `TRI.csv` holds 752 facility-*year* rows for the whole state, i.e. 284
  unique locations, of which 67 lie in the route bounding box. De-duplicate on coordinates
  before counting. `shiny_app/prep_app_data.R` had the same bug and plotted all 752.
- **Runner pass/fail checks** read `figures.log` rather than the per-group console logs (see
  "One command"); a run with failed groups previously reported success.
- **`06_merge_with_wind.R`** now reports how often the nearest met station had no wind in the
  sampling hour and a farther one was used: 218,527 of 2,555,285 rows (8.6%).

### Earlier
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
