# Methane module

Incorporates the CDPHE methane data (Feb 2023 - Sep 2025; same Picarro as H2S)
into the corrected-delay pipeline. Delays applied: **CAT 21 s, EMU 17 s** —
identical to H2S because CH4 comes from the same instrument and inlet.

Data source: `/Users/priyanka/Toxics_EST/MethaneData/` (11 quarterly folders,
299 deployment CSVs; tab-separated with CR line endings — M01 handles this).

## Run order (after R00-R02 of the main rerun pipeline)

| Script | What it does | Key diagnostics |
|--------|--------------|-----------------|
| `M01_ingest_delay_garage.R` | Reads all 299 CSVs, UTC→MST, applies delays, drops no-GPS rows and the ~100 m garage radius around ATOPs HQ (39.785359, -105.104331), 1-s aggregation → `mobile_methane.RData` | file parse check; asset-vs-filename check; timezone window check; applied-shift check; garage fraction per file; CH4 plausibility (~2 ppm baseline) |
| `M02_wind_background.R` | Joins wind from the corrected toxics dataset (Asset+second, hourly fallback), rolling lowest-20th-pct background → `mobile_methane_wind_bg.RData` | wind join rate; background ≈1.9-2.2 ppm; **CH4-vs-H2S lag check (must peak at 0 s — proves CH4 and H2S delays are consistent)** |
| `M03_hotspots.R` | 99th-pct events → DBSCAN (100 m) → 10%/10% persistence → centroids; compares CH4 hotspots to the 17 multi-pollutant groups | thresholds; cluster funnel; eps/threshold sensitivity grid; distance to existing groups |
| `M04_sourceprob_map.R` | Source-probability surface with the manuscript's exact parameters (15 km rays, exp(-d/12 km), σ=900 m) | max-probability location to check against known CH4 sources |

## Outputs (in Downloads/Suncor/)

`mobile_methane.RData/.csv`, `mobile_methane_wind_bg.RData`, `hs_df_methane.RData`,
`cent_out_methane_all.csv`, `cent_out_methane_persistent.csv`,
`methane_hotspot_summary.csv`, `methane_sourceprob.RData`, `methane_sourceprob_map.png`.

## Caveats to carry into the manuscript/SI

- Methane is **not formally calibrated**: one informal check (2025-06-26) recovered
  96.4-97.8% of a 5 ppm standard (within ~5%). State this wherever CH4 is used;
  it argues for reporting *enhancements* and *relative* spatial patterns, not
  absolute-accuracy claims.
- Garage-air exclusion (100 m of ATOPs HQ) should be described in the SI QA/QC section.
- CH4 is reported in **ppm** (aromatics/H2S/HCN are ppb) — keep units explicit in figures.
- Routes include CollinsAerospace deployments in addition to SuncorP66 and
  HEPTerminal — decide whether to include them in the study domain.
