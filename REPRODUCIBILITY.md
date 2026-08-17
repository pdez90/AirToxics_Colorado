# Reproducibility manifest

**Policy (adopted 2026-08-15): every manuscript number must be reproducible from primary
inputs by `RUN_ALL_from_raw.R`. No hand-made or interactive intermediate is accepted.**

```bash
cd /Users/priyanka/Downloads/Suncor/rerun_pipeline
CLEAN=1 Rscript RUN_ALL_from_raw.R 2>&1 | tee logs/RUN_ALL_console.txt
```

`CLEAN=1` quarantines all pre-existing intermediates first (to `quarantine_intermediates_<ts>/`),
so the run provably regenerates everything. Runtime ~4-6 h (HRRR join dominates; fast when
`hrrr_hour_cache/` is warm — that cache holds raw NOAA fields keyed by UTC hour, an input cache,
not a derived product).

## Primary inputs (the only accepted data)

| Input | Location | Source |
|---|---|---|
| Mobile air-toxics quarterly packets (20) | `Updated/*.xlsx` | **Official CDPHE repository** (colorado.gov/airquality/air_toxics_repo.aspx): 2023 Q1-2024 Q2 = _r3, 2024 Q3-2025 Q2 = _r2 — verified exact match to the posted revisions (R00a). 2025 Q3 posted but outside study period. |
| Mobile monthly CSVs (58) | `Updated/csv/{Suncor,Terminal}_<Month>_<Year>.csv` | Derived from the packets above (what script 02 reads). Coverage verified complete Feb 2023-Jun 2025 (R00a); content check vs xlsx via `DEEP=1`; regenerable via `REBUILD=1` (closing the last hand-made step). |
| Methane deployment CSVs (299) | `~/Toxics_EST/MethaneData/<quarter>/` | CDPHE direct (Anna, ATOPs) — the only input NOT from the public repository; Picarro, uncalibrated (one check 96.4-97.8% of 5 ppm). M01 truncates to the study period (<= 2025-06-30) by default. |
| Hourly wind | `hourly_WIND_2023/2024/2025.csv` | EPA AQS Air Data |
| La Casa stationary | `ascent_2023.csv`, `ascent_2024.csv`, `lacasa3.csv` | CDPHE / ASCENT |
| AirToxScreen 2020 | `airtoxscreen.xlsx` (+ `.csv` for the Population column) | EPA |
| TRI | `TRI.csv` | EPA |
| Census blocks | fetched live (`tigris::blocks("08", 2020)`) | US Census |
| HRRR meteorology | fetched live via Herbie (AWS), cached in `hrrr_hour_cache/` | NOAA |
| Grids (500 m, 5 km) | `Grid_500m_generated/`, `grid5km_generated/` — **generated from scratch by `R00b_make_grids.R`** (UTM 13N, cells snapped to absolute 500 m/5 km multiples over documented domain corners −105.25..−104.70, 39.60..40.00). No legacy shapefile used. | fully derived |

## Pipeline DAG (each stage = one wrapper, own R process, with diagnostics)

R00b grids → R01 (raw CSVs → delay-corrected 1-s `mobile.RData`; asset delays CAT 4/6/21 s,
EMU 5/3/17 s, unit-tested + lag-verified) → R02 (wind merge) → R03 (background correction →
500 m segments; Fig 2) → R04 (La Casa scaling → census blocks) → **R04b (canonical block risk)**
→ R05 (source-probability maps + hotspot groups; Figs 3-4) → R06 (HRRR + WWTF + stability) →
R07 (H2S plume ID + Gaussian inversion; Sec 3.6) → M01-M04 (methane) → R99 (numbers report).

## Canonical definitions (decisions locked in for the revision)

1. **Delays**: measured, asset-specific (CAT: aromatics 4 s, HCN 6 s, H2S 21 s; EMU: 5/3/17 s).
2. **Block-level benzene risk** = population-weighted, using `sBenzene_med_of_daily_med_scaled`
   (median of daily medians, La Casa bin-weighted scaling ×1.149), on blocks with AirToxScreen
   benzene + population > 0. Implemented in R04b + script 20. **This replaces the manuscript's
   irreproducible Feb-2026 numbers** (1,120 blocks / 2.4×): the March 2026 block aggregation was
   interactive, left no code or history, and no tested reconstruction (10 candidates) reproduces
   it. Reproducible result: mobile ≈ AirToxScreen population-weighted risk (ratio ~0.97-1.00,
   ~1,668 blocks / ~126,600 people), while block-level spatial correlation remains ~0 with
   order-of-magnitude differences in individual blocks → reframe: "screening models approximate
   the aggregate but misplace it spatially."
3. **Plume inversion**: WWTP-updated funnel (33 flagged events → 4 retained under corrected
   delays); baseline rates 500-2,529 t/yr (mean ~1,180). The manuscript's "137 candidates"
   traces to a pre-WWTP analysis vintage and must be replaced by the reproducible funnel.
4. **Hotspots**: 17 persistent multi-pollutant groups (robust to the delay fix).

## Orphan artifacts (quarantined by CLEAN; produced by no script, consumed by no script)

`hs_df_{benzene,toluene,trimethylbenzene,xylene,h2s,hcn}.RData`, `res_h2s.csv`,
`lacasa_pbl.RData` — leftovers of pre-scripted interactive analyses. Nothing in the pipeline
reads them; they must not be cited for manuscript numbers.

## Known exclusions

- `40_alert.R` (alerts add-on) reads `Suncor_alerts.csv` + orphan `lacasa_pbl.RData`; not part
  of any manuscript number → excluded from RUN_ALL. Rebuild lacasa_pbl from HRRR if ever needed.
- `P09/P10` plume simulations are synthetic-only (no data inputs); run on demand.
- Figure-only scripts (07-09, 15-16, 19, 21-22, 24-25, 31-37) are run after RUN_ALL when
  regenerating manuscript figures; they consume only pipeline outputs.

## Fixes applied for reproducibility

- `R_scripts/18_...R`: now loads `mobile_corrected.RData` from the Suncor folder (was:
  `~/Downloads/` root — stale-file hazard) and normalizes the `out_sf`/`out_merge` object name.
- `block_sf_risk` (script 20's input, formerly interactive) is now constructed by `R04b`.
- Grids are now GENERATED from scratch (`R00b_make_grids.R`); scripts 08/12/13/14 read the
  generated paths. The legacy QGIS shapefiles (`Grid_500m/`, `grid5km/`) are no longer consumed.
  Note: cell boundaries shift vs the legacy grid, so segment ids/Fig 2 values change slightly.
- Methane: CR-only line endings handled; garage filter (100 m of ATOPs HQ) scripted; delays same
  as H2S (same Picarro).
