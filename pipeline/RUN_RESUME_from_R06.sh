#!/bin/bash
# ==============================================================
# RUN_RESUME_from_R06.sh   (2026-09-23)
# Picks the full re-run up where it stopped, WITHOUT redoing R01-R05.
#
#   bash ~/Downloads/Suncor/rerun_pipeline/RUN_RESUME_from_R06.sh
#
# Why this exists: the 10:40 run completed R01-R05 in 52 min, then R06 hung
# for 6h15m in the HRRR join. P04 used plan(multicore) - a fork - and each
# forked worker called into Python (Herbie/xarray/cfgrib), which deadlocks.
# P04 now runs the fetch sequentially and P03 prints progress every 25
# hour-groups. R01-R05 outputs are on disk and correct, so there is no reason
# to spend another hour regenerating them.
#
# There is deliberately NO CLEAN here: CLEAN quarantines intermediates, which
# is exactly what we are trying to keep.
#
# Stages:
#   1  R06 HRRR + WWTF + stability     ~1-2 h  (sequential HRRR fetch)
#   2  R07 plume inversion             ~10 min
#   3  methane M01-M04                 ~30 min
#   4  R99 manuscript numbers          ~5 min
#   5  P09 / P10 plume simulations     ~20 min
#   6  MAKE_FIGURES.R, all groups      ~13-16 h
#   7  impact diagnostic               ~2 min
#
# Set HRRR_PARALLEL=1 to try the multisession HRRR path instead of sequential.
# Note that fresh workers may not inherit the r-reticulate virtualenv, so if
# they cannot import herbie, unset it and run sequentially.
# ==============================================================
set -u
BASE="$HOME/Downloads/Suncor"
PIPE="$BASE/rerun_pipeline"
LOGDIR="$PIPE/logs/resume_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOGDIR"
cd "$BASE" || exit 1

echo "=============================================================="
echo " Suncor resume from R06   $(date)"
echo " logs: $LOGDIR"
echo "=============================================================="

FAILED=()
run_stage () {
  local label="$1"; shift
  local log="$1"; shift
  echo; echo "-------- $label --------"; echo "         log: $log"
  local t0=$SECONDS
  if caffeinate -i "$@" > "$log" 2>&1; then
    printf '         OK   (%d min)\n' $(( (SECONDS - t0) / 60 ))
  else
    printf '         FAIL (%d min)  <-- see the log\n' $(( (SECONDS - t0) / 60 ))
    FAILED+=("$label")
    tail -20 "$log" | sed 's/^/         | /'
  fi
}

# ---- pre-flight: the R01-R05 outputs this resume depends on ----
echo; echo "-------- pre-flight --------"
missing=0
for f in mobile_wswd.RData bgcorrected_out_merge.RData \
         lacasa_scaling_factors_option1_binweighted.RData \
         group_summary_persistent.csv hotspot_thresholds_summary.csv; do
  if [ -f "$BASE/$f" ]; then
    echo "         OK   $f ($(date -r "$BASE/$f" '+%m-%d %H:%M'))"
  else
    echo "         MISSING: $f"; missing=1
  fi
done
if [ "$missing" = 1 ]; then
  echo "         R01-R05 outputs are not all present - run RUN_EVERYTHING.sh instead."
  exit 1
fi

run_stage "1: R06 HRRR + WWTF + stability  ~1-2 h" "$LOGDIR/R06.log" \
          Rscript "$PIPE/R06_hrrr_plume_prep.R"
# R07 cannot run without R06's output, so do not pretend otherwise.
if [ -f "$BASE/mobile_hrrr_windfromwwtf_stability_filtered.RData" ]; then
  run_stage "2: R07 plume inversion" "$LOGDIR/R07.log" \
            Rscript "$PIPE/R07_plume_inversion.R"
else
  echo; echo "-------- 2: R07 plume inversion --------"
  echo "         SKIPPED: R06 did not produce mobile_hrrr_windfromwwtf_stability_filtered.RData"
  FAILED+=("2: R07 (skipped, no R06 output)")
fi

for m in M01_ingest_delay_garage M02_wind_background M03_hotspots M04_sourceprob_map; do
  run_stage "3: methane $m" "$LOGDIR/$m.log" Rscript "$PIPE/methane/$m.R"
done

run_stage "4: R99 manuscript numbers" "$LOGDIR/R99.log" \
          Rscript "$PIPE/R99_manuscript_numbers_report.R"

if [ -f "$BASE/mobile_hrrr.RData" ]; then
  run_stage "5a: P09 stack-height simulation" "$LOGDIR/P09.log" \
            Rscript "$PIPE/plume_scripts/P09_simulations_real_stack_height_varies.R"
  run_stage "5b: P10 crosswind simulation" "$LOGDIR/P10.log" \
            Rscript "$PIPE/plume_scripts/P10_simulations_cross_wind_distance_0.R"
else
  echo; echo "-------- 5: plume simulations --------"
  echo "         SKIPPED: no mobile_hrrr.RData"
  FAILED+=("5: P09/P10 (skipped, no mobile_hrrr.RData)")
fi

run_stage "6: MAKE_FIGURES.R, all groups  ~13-16 h" "$LOGDIR/figures.log" \
          env R_MAX_VSIZE=100Gb Rscript "$PIPE/MAKE_FIGURES.R"

echo; echo "-------- per-group figure result --------"
# BUGFIX 2026-09-25: this used to grep each FIG_<g>_console.txt for its own
# "[FAIL] group <g>" line. MAKE_FIGURES writes those PASS/FAIL lines to
# figures.log, NOT into the per-group console logs, so the grep never matched:
# gfail stayed 0, nothing was appended to FAILED, and the run printed
# "all groups completed" and "ALL STAGES COMPLETED" even when groups had
# failed. The 2026-09-23 run reported exactly that with U and A both failed -
# the same false all-clear this check was added to prevent. Read figures.log.
gfail=0
while read -r g; do
  [ -n "$g" ] || continue
  echo "         FAIL group $g   (log: logs/FIG_${g}_console.txt)"
  FAILED+=("fig_$g"); gfail=1
done < <(grep -aE '^[[:space:]]*\[FAIL\] group ' "$LOGDIR/figures.log" 2>/dev/null \
         | awk '{print $3}' | sort -u)
# also catch a group that started but never produced a result line at all
_started=$(grep -ac 'DIAG | GROUP ' "$LOGDIR/figures.log" 2>/dev/null || echo 0)
_ended=$(grep -acE '^[[:space:]]*\[(PASS|FAIL)\] group ' "$LOGDIR/figures.log" 2>/dev/null || echo 0)
if [ "$_started" -ne "$_ended" ]; then
  echo "         WARNING: $_started group(s) started but $_ended result line(s) written"
  FAILED+=("fig_incomplete"); gfail=1
fi
[ "$gfail" = 0 ] && echo "         all groups completed"

run_stage "7: impact diagnostic" "$LOGDIR/impact.log" \
          env SUNCOR_BASE="$BASE" Rscript tests/impact_of_time_fixes.R

echo; echo "=============================================================="
if [ ${#FAILED[@]} -eq 0 ]; then
  echo " ALL STAGES COMPLETED"
else
  echo " ${#FAILED[@]} STAGE(S) FAILED:"
  for f in "${FAILED[@]}"; do echo "   - $f"; done
fi
echo
echo " manuscript numbers : $PIPE/manuscript_numbers_old_vs_new.csv"
echo " plume funnel       : $BASE/WWTP_H2S_plume_funnel.csv"
echo " logs               : $LOGDIR"
echo
echo " Then: Rscript $BASE/shiny_app/prep_app_data.R  to refresh the app data."
