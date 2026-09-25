#!/bin/bash
# ==============================================================
# RUN_HQ300.sh  (2026-09-22)
# Re-run the analysis with every measurement within 300 m of CDPHE's
# ATOPs headquarters (39.785189, -105.104411) removed, as requested by
# the CDPHE co-authors. The exclusion itself lives in
# R_scripts/03_checks_flags.R (section 3c) so every downstream step
# inherits it.
#
# PHASE 1 (this script) needs only the inputs that are on disk now
# (CDPHE monthly CSVs + EPA AQS wind):
#   R01 delay + QA/QC + HQ exclusion  ->  R02 wind  ->  R03 background +
#   500 m segments  ->  R05 source probability + hotspot groups  ->
#   Figure 2 (script 55)  ->  before/after report.
# The Gaussian plume stages (R06, R07) are NOT run: all four retained plumes
# and every plume candidate lie 0.5-5 km from the wastewater facility, more
# than 9 km from HQ, so the exclusion cannot touch them.
#
# Usage:   bash ~/Downloads/Suncor/rerun_pipeline/RUN_HQ300.sh
# ==============================================================
set -uo pipefail
BASE="$HOME/Downloads/Suncor"
PIPE="$BASE/rerun_pipeline"
STAMP=$(date +%Y%m%d_%H%M%S)
LOG="$PIPE/logs/hq300_$STAMP"
mkdir -p "$LOG"
cd "$BASE" || { echo "cannot cd to $BASE"; exit 1; }
say() { echo "[$(date +%H:%M:%S)] $*" | tee -a "$LOG/RUN_HQ300.log"; }

say "HQ exclusion re-run, logs in $LOG"

# ---- 0) pre-flight -----------------------------------------------------
n_csv=$(ls "$BASE/Updated/csv" 2>/dev/null | grep -cE '^(Suncor|Terminal)_.*\.csv$')
say "CDPHE monthly CSVs: $n_csv (need 58)"
[ "$n_csv" -ge 55 ] || { say "STOP: monthly CSVs missing"; exit 1; }
for y in 2023 2024 2025; do
  [ -f "$BASE/hourly_WIND_$y.csv" ] || { say "STOP: hourly_WIND_$y.csv missing"; exit 1; }
done
grep -q "HQ_RADIUS_M <- 300" "$BASE/R_scripts/03_checks_flags.R" \
  || { say "STOP: 03_checks_flags.R does not carry the HQ exclusion"; exit 1; }
say "03_checks_flags.R carries the 300 m HQ exclusion: OK"

say "checking R packages (installs any that are missing from CRAN)"
Rscript -e '
pk <- c("data.table","dplyr","lubridate","zoo","slider","stringr","readr","sf",
        "geosphere","dbscan","terra","ggplot2","ggspatial","patchwork","scales",
        "tibble","tidyr","purrr","raster","rosm","prettymapr")
miss <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) { message("installing: ", paste(miss, collapse = ", "))
  install.packages(miss, repos = "https://cloud.r-project.org") }
still <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)]
if (length(still)) stop("could not install: ", paste(still, collapse = ", "))
message("all packages present")' >> "$LOG/packages.log" 2>&1 \
  || { say "STOP: package installation failed - see $LOG/packages.log"; exit 1; }
say "packages: OK"

# keep the pre-exclusion analysis set for reference (not overwritten on re-runs)
mkdir -p "$BASE/pre_hq300"
for f in mobile_wswd.RData segment500_summaries_acrossSites.RData; do
  [ -f "$BASE/$f" ] && [ ! -f "$BASE/pre_hq300/$f" ] && cp "$BASE/$f" "$BASE/pre_hq300/$f"
done

# ---- 1) pipeline stages --------------------------------------------------
run() {  # run <label> <script>
  local t0=$(date +%s)
  say "START $1"
  if caffeinate -i Rscript "$2" > "$LOG/$1.log" 2>&1; then
    say "DONE  $1 ($(( ($(date +%s) - t0) / 60 )) min)"
  else
    say "FAIL  $1 - last lines of $LOG/$1.log:"
    tail -25 "$LOG/$1.log" | tee -a "$LOG/RUN_HQ300.log"
    exit 1
  fi
}
run R01_delay_qaqc_HQ   "$PIPE/R01_delay_reprocessing.R"
grep -h "\[HQ\]" "$LOG/R01_delay_qaqc_HQ.log" | tee -a "$LOG/RUN_HQ300.log"
run R02_wind            "$PIPE/R02_wind_merge.R"
run R03_background      "$PIPE/R03_background_segments.R"
run R05_hotspots        "$PIPE/R05_hotspots.R"
run Fig2_script55       "$BASE/R_scripts/55_figure2_sharedscale.R"

# ---- 2) what changed -----------------------------------------------------
run before_after        "$PIPE/HQ300_before_after.R"
cp "$BASE/HQ300_before_after.txt" "$LOG/" 2>/dev/null
say "ALL PHASE-1 STAGES COMPLETE"
say "report: $BASE/HQ300_before_after.txt"
