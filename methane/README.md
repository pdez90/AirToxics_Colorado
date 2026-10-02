# Methane module

Incorporates the CDPHE methane data (Feb 2023 - Sep 2025; same Picarro as H2S)
into the corrected-delay pipeline. Delays applied: **CAT 21 s, EMU 17 s** —
identical to H2S because CH4 comes from the same instrument and inlet.

Data source: the `MethaneData/` folder obtained from CDPHE (11 quarterly folders, 299
deployment CSVs; tab-separated with CR line endings — M01 handles this). `RUN_EVERYTHING.sh`
looks for it in `~/Downloads/MethaneData` or `$SUNCOR_BASE/MethaneData`; set `METHANE_DIR`
otherwise.

## Run order (after R00-R02 of the main rerun pipeline)

| Script | What it does | Key diagnostics |
|--------|--------------|-----------------|
| `M01_ingest_delay_garage.R` | Reads all 299 CSVs, UTC→MST, applies delays, drops no-GPS rows, averages within 5-s clock bins, removes the 300 m radius around the ATOPs headquarters (39.785189, -105.104411; the same screen as the air toxics) and keeps one value per bin → `mobile_methane.RData` | file parse check; asset-vs-filename check; timezone window check; applied-shift check; garage fraction per file; CH4 plausibility (~2 ppm baseline) |
| `M02_wind_background.R` | Joins wind from the corrected toxics dataset (Asset+second, hourly fallback), rolling lowest-20th-pct background → `mobile_methane_wind_bg.RData` | wind join rate; background ≈1.9-2.2 ppm; **CH4-vs-H2S lag check (must peak at 0 s — proves CH4 and H2S delays are consistent)** |
| `M03_hotspots.R` | 99th-pct events → DBSCAN (100 m, minPts 5) → 10%/10% persistence → centroids; compares CH4 hotspots to the 15 multi-pollutant groups | thresholds; cluster funnel; eps/threshold sensitivity grid; distance to existing groups |
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
- The 300 m headquarters exclusion is described in SI section S1.4 and applies to CH4 as well.
- CH4 is reported in **ppm** (aromatics/H2S/HCN are ppb) — keep units explicit in figures.
- Routes include Collins Aerospace (Goodrich) deployments in addition to SuncorP66 and
  HEPTerminal; those are excluded from the study domain, as for the air toxics.
