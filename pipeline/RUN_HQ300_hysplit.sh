#!/bin/bash
# ==============================================================
# RUN_HQ300_hysplit.sh   (2026-09-23)
# Figure S4.8 - the only figure group that has not been re-run on the
# HQ-excluded data. Group I was skipped throughout the HQ300 re-run because
# it needs HYSPLIT meteorology, which splitr downloads at run time.
#
#   bash ~/Downloads/Suncor/rerun_pipeline/RUN_HQ300_hysplit.sh
#
# What it does: 36_hysplit_of_lowest_trimethylbenzene_benzene_ratios.R reads
# the CURRENT mobile_wswd.RData, takes the bottom 20% of the TMB/benzene
# ratio, thins to one observation per UTC hour, samples up to 60 receptors
# and runs 24-h backward trajectories (reanalysis met, 30 m AGL).
#
# The exclusion shifts the bottom-20% threshold, so the receptor SET changes
# even though the count stays at 60 unless the thinned pool falls below that.
# Read the new count off the "Running trajectories for N = ..." line; if it is
# not 60, SI Figure S4.8's caption ("60 mobile-monitoring observations")
# needs updating.
#
# Requirements, in order of likelihood of being the thing that stops you:
#   - splitr    NOT on CRAN; installed from GitHub below.
#   - network   splitr downloads reanalysis .gbl met files per trajectory
#               month into a temp directory. First run is the slow one.
#   - webshot2  + a Chrome/Chromium install, for the leaflet screenshot.
# Runtime: roughly 20-60 min for 60 trajectories, dominated by met download.
# ==============================================================
BASE="$HOME/Downloads/Suncor"
PIPE="$BASE/rerun_pipeline"
LOG="$PIPE/logs/hq300_hysplit_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOG"; cd "$BASE" || exit 1
say() { echo "[$(date +%H:%M:%S)] $*" | tee -a "$LOG/HYSPLIT.log"; }

say "checking packages (splitr comes from GitHub, not CRAN)"
Rscript -e '
pk <- c("remotes","webshot2","leaflet","htmlwidgets","ggspatial","sf","scales","dplyr","lubridate","tidyr","ggplot2")
m <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)]
if (length(m)) install.packages(m, repos = "https://cloud.r-project.org")
if (!requireNamespace("splitr", quietly = TRUE)) remotes::install_github("rich-iannone/splitr")
cat("splitr available:", requireNamespace("splitr", quietly = TRUE), "\n")' \
  > "$LOG/packages.log" 2>&1
tail -2 "$LOG/packages.log" | sed 's/^/        | /' | tee -a "$LOG/HYSPLIT.log"

if ! grep -q "splitr available: TRUE" "$LOG/packages.log"; then
  say "STOP: splitr did not install. See $LOG/packages.log"
  exit 1
fi

t0=$(date +%s)
say "START group I (Figure S4.8)"
caffeinate -i env GROUPS="I" RUN_HYSPLIT=1 R_MAX_VSIZE=100Gb \
  Rscript "$PIPE/MAKE_FIGURES.R" > "$LOG/fig_I.log" 2>&1
mins=$(( ($(date +%s) - t0) / 60 ))

if grep -q "^  \[FAIL\] group I" "$LOG/fig_I.log"; then
  say "FAIL  group I ($mins min)"
  tail -15 "$PIPE/logs/FIG_I_console.txt" 2>/dev/null | sed 's/^/        | /' | tee -a "$LOG/HYSPLIT.log"
else
  say "DONE  group I ($mins min)"
fi

say "--- receptor count for the Figure S4.8 caption ---"
grep -h "Running trajectories for N\|Rows in bottom\|Bottom 20% threshold" \
  "$LOG/fig_I.log" "$PIPE/logs/FIG_I_console.txt" 2>/dev/null | sort -u | sed 's/^/        | /' | tee -a "$LOG/HYSPLIT.log"
say "logs: $LOG"
