#!/bin/bash
# ==============================================================
# RUN_EVERYTHING.sh
# Raw CDPHE data in -> every manuscript number, figure and simulation out.
#
#   bash ~/Downloads/Suncor/rerun_pipeline/RUN_EVERYTHING.sh
#
# Stages, in order:
#   0  guard tests            ~1 min    fail here and nothing else runs
#   1  RUN_ALL_from_raw.R     ~4-6 h    every intermediate, from raw CSVs
#   2  P09 / P10 simulations  ~20 min   need mobile_hrrr.RData from stage 1
#   3  MAKE_FIGURES.R         ~1-2 h    all manuscript + SI figures, incl.
#                                       group I = script 36 (HYSPLIT)
#   4  impact diagnostic      ~2 min    what the time fixes moved
#
# Everything is logged to rerun_pipeline/logs/run_<timestamp>/. The script
# keeps going after a stage fails and prints a pass/fail summary at the end,
# EXCEPT stage 0: if the guard tests fail, nothing else runs, because a
# multi-hour run under a broken time convention is worse than no run.
#
# Resume after a failure: comment out the stages that already succeeded, or
# run the single Rscript line for the stage you want. Stage 1 is the only one
# that quarantines intermediates (CLEAN=1), so re-running stages 2-4 alone is
# safe and does not destroy stage 1's output.
# ==============================================================
set -u

BASE="$HOME/Downloads/Suncor"
PIPE="$BASE/rerun_pipeline"
STAMP="$(date +%Y%m%d_%H%M%S)"
LOGDIR="$PIPE/logs/run_$STAMP"
mkdir -p "$LOGDIR"

echo "=============================================================="
echo " Suncor full re-run   $(date)"
echo " logs: $LOGDIR"
echo "=============================================================="

FAILED=()
run_stage () {           # run_stage <label> <logfile> <command...>
  local label="$1"; shift
  local log="$1"; shift
  echo
  echo "-------- $label --------"
  echo "         log: $log"
  local t0=$SECONDS
  if "$@" > "$log" 2>&1; then
    printf '         OK   (%d min)\n' $(( (SECONDS - t0) / 60 ))
  else
    printf '         FAIL (%d min)  <-- see the log\n' $(( (SECONDS - t0) / 60 ))
    FAILED+=("$label")
    tail -20 "$log" | sed 's/^/         | /'
  fi
}

# --------------------------------------------------------------
# 0) Guard tests. These are cheap and they gate everything else.
# --------------------------------------------------------------
echo
echo "-------- stage 0: guard tests --------"
cd "$BASE" || exit 1
# SUNCOR_CODE_DIRS is deliberately NOT set: the test's own default list covers
# both layouts (R_scripts, pipeline, plume_scripts, hrrr_scripts, methane,
# rerun_pipeline). Overriding it with a narrower list made the suite silently
# SKIP the H04 and P04 checks in a repository checkout - and still exit 0, so
# the gate went green having tested nothing.
#
# SUNCOR_STRICT=1 turns "no data found, skipping" into a failure. This is the
# gate for a 6-9 hour run; a pass that means "I could not find anything to
# check" is worse than no gate at all.
if SUNCOR_STRICT=1 SUNCOR_CSV_DIR="$BASE/Updated/csv" SUNCOR_BASE="$BASE" \
     Rscript tests/test_time_convention.R > "$LOGDIR/00_test_time.log" 2>&1 \
   && Rscript tests/test_p08_geometry.R > "$LOGDIR/00_test_geometry.log" 2>&1; then
  echo "         OK   time convention + P08 geometry"
else
  echo "         FAIL - stopping before the long run."
  echo
  cat "$LOGDIR/00_test_time.log" "$LOGDIR/00_test_geometry.log" 2>/dev/null | tail -40
  exit 1
fi

# --------------------------------------------------------------
# 1) The pipeline itself, from raw inputs.
#    CLEAN=1 quarantines every existing intermediate first, so the run
#    provably regenerates everything and nothing stale can leak in.
# --------------------------------------------------------------
cd "$PIPE" || exit 1
run_stage "stage 1: RUN_ALL_from_raw.R (CLEAN=1)  ~4-6 h" \
          "$LOGDIR/01_run_all.log" \
          env CLEAN=1 Rscript "$PIPE/RUN_ALL_from_raw.R"

# --------------------------------------------------------------
# 2) Plume simulations. Separate from stage 1 because they are validation
#    runs on synthetic sources; they read only the met fields from
#    mobile_hrrr.RData, which stage 1 regenerates.
# --------------------------------------------------------------
run_stage "stage 2a: P09 stack-height simulation" \
          "$LOGDIR/02a_P09.log" \
          Rscript "$PIPE/plume_scripts/P09_simulations_real_stack_height_varies.R"

run_stage "stage 2b: P10 crosswind simulation" \
          "$LOGDIR/02b_P10.log" \
          Rscript "$PIPE/plume_scripts/P10_simulations_cross_wind_distance_0.R"

# --------------------------------------------------------------
# 3) Figures. Group I is script 36, the HYSPLIT back-trajectories - the one
#    output whose numbers actually move under the time fixes (35 of 60
#    receptors were being launched an hour early).
# --------------------------------------------------------------
run_stage "stage 3: MAKE_FIGURES.R (incl. HYSPLIT)  ~1-2 h" \
          "$LOGDIR/03_figures.log" \
          Rscript "$PIPE/MAKE_FIGURES.R"

# --------------------------------------------------------------
# 4) What moved.
# --------------------------------------------------------------
cd "$BASE" || exit 1
run_stage "stage 4: impact diagnostic" \
          "$LOGDIR/04_impact.log" \
          env SUNCOR_BASE="$BASE" Rscript tests/impact_of_time_fixes.R

# --------------------------------------------------------------
echo
echo "=============================================================="
if [ ${#FAILED[@]} -eq 0 ]; then
  echo " ALL STAGES COMPLETED"
else
  echo " ${#FAILED[@]} STAGE(S) FAILED:"
  for f in "${FAILED[@]}"; do echo "   - $f"; done
fi
echo
echo " manuscript numbers : $PIPE/manuscript_numbers_old_vs_new.csv"
echo " plume inversion    : $BASE/FinalFig/WWTP_H2S_inversion_*_METRIC_TPY.csv"
echo " plume funnel       : $BASE/WWTP_H2S_plume_funnel.csv"
echo " figures            : $BASE/FinalFig/"
echo " logs               : $LOGDIR"
echo
echo " Next: walk the manuscript against manuscript_numbers_old_vs_new.csv."
echo " Expect changes in the retained-plume count (P07 now drops events with no"
echo " stability class) and in the quoted emission range (ill-conditioned"
echo " scenarios are excluded). HYSPLIT figures change because 35 of 60"
echo " receptors were previously launched an hour early."
echo "=============================================================="
[ ${#FAILED[@]} -eq 0 ]
