#!/bin/bash
# ==============================================================
# RUN_HQ300_phase2.sh  (2026-09-22)
# Everything after Phase 1 (RUN_HQ300.sh), on the HQ-excluded data,
# EXCEPT the Gaussian plume stages (R06, R07) and the plume groups V, W.
#
#   bash ~/Downloads/Suncor/rerun_pipeline/RUN_HQ300_phase2.sh
#
# Part A needs no La Casa / AirToxScreen input and always runs:
#   TRI subset (23), roads (38, downloaded once), methane M01-M04,
#   figure groups A C J J3 O Z E F M N G K H L P Q  (+ I if RUN_HYSPLIT=1)
# Part B runs only when ascent_2023.csv AND airtoxscreen.xlsx are present:
#   R04 (La Casa scaling + census blocks), R04b (block risk),
#   groups D B J2 R S T X Y, then R99 (manuscript-numbers report).
# Every stage is logged; a failed stage is reported and the rest continue.
# ==============================================================
# (no set -u: macOS bash 3.2 treats empty arrays as unbound)
BASE="$HOME/Downloads/Suncor"
PIPE="$BASE/rerun_pipeline"
STAMP=$(date +%Y%m%d_%H%M%S)
LOG="$PIPE/logs/hq300_phase2_$STAMP"
mkdir -p "$LOG"
cd "$BASE" || exit 1
say() { echo "[$(date +%H:%M:%S)] $*" | tee -a "$LOG/PHASE2.log"; }
FAILED=(); DONE=()
run() {  # run <label> <command...>
  local label="$1"; shift; local t0=$(date +%s)
  say "START $label"
  if caffeinate -i "$@" > "$LOG/$label.log" 2>&1; then
    say "DONE  $label ($(( ($(date +%s) - t0) / 60 )) min)"; DONE+=("$label")
  else
    say "FAIL  $label - last lines of $LOG/$label.log:"
    tail -15 "$LOG/$label.log" | sed 's/^/        | /' | tee -a "$LOG/PHASE2.log"
    FAILED+=("$label")
  fi
}
fig() {  # MAKE_FIGURES.R exits 0 even when a group fails, so read its own log
  local g="$1"
  run "fig_$g" env GROUPS="$g" R_MAX_VSIZE=100Gb Rscript "$PIPE/MAKE_FIGURES.R"
  if grep -q "^  \[FAIL\] group $g" "$LOG/fig_$g.log" 2>/dev/null; then
    say "FAIL  group $g (the stage exited 0 but the group did not finish) - see $PIPE/logs/FIG_${g}_console.txt"
    tail -6 "$PIPE/logs/FIG_${g}_console.txt" 2>/dev/null | sed 's/^/        | /' | tee -a "$LOG/PHASE2.log"
    FAILED+=("fig_$g")
  fi
}

# ---- pre-flight ------------------------------------------------------------
[ -f "$BASE/HQ300_before_after.txt" ] && [ "$BASE/HQ300_before_after.txt" -nt "$BASE/pre_hq300/mobile_wswd.RData" ] \
  || { say "STOP: Phase 1 has not finished (no fresh HQ300_before_after.txt). Run RUN_HQ300.sh first."; exit 1; }
grep -q "\[PASS\] no analysis-set row lies within 300 m" "$BASE/HQ300_before_after.txt" \
  || { say "STOP: Phase 1 report does not confirm the 300 m exclusion."; exit 1; }
for f in TRI.csv ascent_2024.csv lacasa3.csv; do
  [ -f "$BASE/$f" ] || { say "STOP: $f missing"; exit 1; }
done
PART_B=1
[ -f "$BASE/ascent_2023.csv" ] || { say "NOTE: ascent_2023.csv missing -> Part B (La Casa scaling, blocks, risk) skipped"; PART_B=0; }
if [ ! -f "$BASE/airtoxscreen.xlsx" ]; then
  for c in "$BASE/airtoxscreen_epa_region8_CO.csv" \
           "$BASE/Region8_2020ATS_Ambient_Concentrations.xlsx" \
           "$HOME/Downloads/Region8_2020ATS_Ambient_Concentrations.xlsx" \
           "$HOME/Downloads/Additional_suncor/Region8_2020ATS_Ambient_Concentrations.xlsx"; do
    if [ -f "$c" ]; then run airtoxscreen_rebuild Rscript "$BASE/R_scripts/75_airtoxscreen_from_epa.R"; break; fi
  done
fi
[ -f "$BASE/airtoxscreen.xlsx" ] || { say "NOTE: airtoxscreen.xlsx missing -> Part B skipped"; PART_B=0; }
Rscript -e 'pk <- c("splitr","openair","cowplot","jpeg","tigris","readxl","writexl","gifski","magick","ggrepel","viridis");
  m <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)];
  m <- setdiff(m, "splitr"); if (length(m)) install.packages(m, repos = "https://cloud.r-project.org")' \
  > "$LOG/packages.log" 2>&1

# ---- Part A --------------------------------------------------------------
if [ -z "${SKIP_A:-}" ]; then
say "=== PART A (no La Casa / AirToxScreen needed) ==="
run tri_subset Rscript "$BASE/R_scripts/23_tri.R"
[ -f "$BASE/all_colorado_roads.RData" ] || run roads_download Rscript "$BASE/R_scripts/38_download_roads.R"
run M01_methane Rscript "$PIPE/methane/M01_ingest_delay_garage.R"
run M02_methane Rscript "$PIPE/methane/M02_wind_background.R"
run M03_methane Rscript "$PIPE/methane/M03_hotspots.R"
run M04_methane Rscript "$PIPE/methane/M04_sourceprob_map.R"
for g in A C J J3 O Z E F M N G K H L P Q; do fig "$g"; done
if [ -n "${RUN_HYSPLIT:-}" ]; then fig I; else say "skip group I (HYSPLIT); set RUN_HYSPLIT=1 to include it"; fi
else say "SKIP_A set: Part A skipped"; fi

# ---- Part B --------------------------------------------------------------
if [ "$PART_B" = 1 ]; then
  say "=== PART B (La Casa scaling, census blocks, risk) ==="
  run R04_scaling_census Rscript "$PIPE/R04_scaling_census_risk.R"
  run R04b_block_risk    Rscript "$PIPE/R04b_build_block_sf_risk.R"
  for g in D B J2 R S T X Y; do fig "$g"; done
  run R99_numbers Rscript "$PIPE/R99_manuscript_numbers_report.R"
fi

say "=== SUMMARY ==="
say "succeeded: ${#DONE[@]}   failed: ${#FAILED[@]} ${FAILED[*]:-}"
[ "$PART_B" = 1 ] || say "Part B still to run once ascent_2023.csv and airtoxscreen.xlsx are in $BASE (re-running this script redoes Part A too; run: SKIP_A=1 bash $0)"
say "logs: $LOG"
