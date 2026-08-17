# AirToxics_Colorado

Analysis code for: **Mobile monitoring of air toxics in North Denver / Commerce City, Colorado**
(deSouza et al., manuscript es-2026-04959z). A 230-day CDPHE mobile campaign (Feb 2023 - Jun 2025)
measuring benzene, toluene, trimethylbenzene, xylene, H2S, HCN (and methane) around the HB21-1189
"covered facilities", combined with the La Casa stationary site.

**Reproducibility policy: every manuscript number is regenerated from primary data by one command.**
No interactive or hand-made intermediate products are used. See `REPRODUCIBILITY.md` for the full
input manifest, pipeline DAG, and canonical definitions.

```bash
cd pipeline
CLEAN=1 Rscript RUN_ALL_from_raw.R
```

## Data (not included in this repository)

| Data | Source |
|---|---|
| Mobile air-toxics quarterly packets | [CDPHE Air Toxics repository](https://www.colorado.gov/airquality/air_toxics_repo.aspx) — 2023 Q1-2025 Q2, Suncor & Phillips 66 route and HEP (Sinclair) Terminal route, revisions r3 (2023Q1-2024Q2) / r2 (2024Q3-2025Q2) |
| Methane deployment CSVs | CDPHE ATOPs (obtained directly; not on the public repository) |
| Hourly wind | EPA AQS Air Data |
| La Casa stationary VOCs | CDPHE / ASCENT |
| AirToxScreen 2020, TRI | EPA |
| Census blocks | US Census via `tigris` |
| HRRR meteorology | NOAA via `Herbie` (AWS) |

File paths in the scripts are absolute to the authors' analysis machine
(`/Users/priyanka/Downloads/Suncor/...`); adjust the `BASE` constant in
`pipeline/diagnostics_helpers.R` and the data locations to reproduce elsewhere.

## Layout

- `pipeline/` — orchestration + diagnostics. `RUN_ALL_from_raw.R` verifies primary inputs
  (`R00a`), generates analysis grids from scratch (`R00b`), then runs the numbered stages
  R01-R07 (each sources the corresponding section scripts and prints/logs diagnostics:
  unit tests of the sampling-delay correction, lag verification, old-vs-new comparisons,
  physical sanity checks). `R04c`-`R04f` are the forensic scripts used to audit a legacy
  interactive analysis (kept for the record). `R99` writes the manuscript-numbers report.
- `R_scripts/` — the analysis itself, one file per section (split from the original Rmd):
  ingest (02), QA + asset-specific inlet-delay correction (03; CAT: aromatics 4 s, HCN 6 s,
  H2S 21 s; EMU: 5/3/17 s), EPA wind merge (05-06), rolling background correction (10-11),
  500 m segments (12-14), La Casa temporal scaling (17), census-block estimates + AirToxScreen
  comparison and benzene cancer risk (18, 20), hotspot/source-probability analyses (26-30),
  figures (07-09, 15-16, 19, 21+, 31-37).
- `plume_scripts/` — H2S plume identification and Gaussian-plume inversion (P07-P08) with
  HRRR meteorology, WWTF wind alignment, stability classes (P03-P06) and simulation-based
  bias/sensitivity experiments (P09-P10).
- `hrrr_scripts/` — faster cached HRRR point-sampling variant.
- `methane/` — CH4 module (same Picarro as H2S; delays CAT 21 s / EMU 17 s; garage-air filter;
  hotspots + source-probability surface). See `methane/README.md` for QA caveats.

## Key methodological notes

- Sampling delays are measured and platform-specific; the correction is unit-tested and
  verified by lag-recovery diagnostics (`pipeline/R01_delay_reprocessing.R`).
- The 500 m / 5 km analysis grids are generated deterministically (UTM 13N, absolute-multiple
  snapping) — no legacy GIS artifacts (`pipeline/R00b_make_grids.R`).
- Block-level benzene risk = population-weighted, median of daily medians, La Casa
  bin-weighted temporal scaling, on census blocks with AirToxScreen benzene and population > 0.
