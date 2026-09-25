#!/bin/bash
# ==============================================================
# RUN_EVERYTHING.sh
# Raw CDPHE data in -> every manuscript number, figure and simulation out.
#
#   bash ~/Downloads/Suncor/rerun_pipeline/RUN_EVERYTHING.sh
#
# Stages, in order:
#   0  guard tests            ~1 min    fail here and nothing else runs
#   1  RUN_ALL_from_raw.R     ~5-7 h    every intermediate, from raw CSVs,
#                                       INCLUDING the 300 m ATOPs-headquarters
#                                       exclusion (03_checks_flags.R section 3c)
#                                       and the HRRR + Gaussian plume branch
#   2  P09 / P10 simulations  ~20 min   need mobile_hrrr.RData from stage 1
#   3  MAKE_FIGURES.R         ~13-16 h  ALL groups. Groups A and L dominate:
#                                       measured 402 min and 256 min on
#                                       2026-09-22/23, and both need a raised
#                                       vector limit (set below).
#   4  impact diagnostic      ~2 min    what the time fixes moved
#
# Budget the better part of a day, and leave the machine plugged in; every
# stage runs under caffeinate where it matters.
#
# UPDATED 2026-09-23:
#   - R_MAX_VSIZE is now set for stage 3. Without it groups A and L die with
#     "vector memory exhausted", which is how they failed in the September run.
#   - splitr is installed up front. It is NOT on CRAN, and group I (script 36,
#     the HYSPLIT trajectories behind Figure S4.8) cannot run without it.
#   - a pre-flight check refuses to start if plume_scripts/ or hrrr_scripts/
#     are missing, because R06 sources them and a 6-hour stage 1 that dies at
#     the plume branch wastes the whole run.
#   - SKIP_PLUMES is explicitly unset: this driver runs the plume chain.
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
# --------------------------------------------------------------
# 0a) Pre-flight: the plume chain must be present, and splitr installed.
# --------------------------------------------------------------
echo
echo "-------- stage 0a: pre-flight --------"
missing=0
for d in plume_scripts hrrr_scripts; do
  if [ ! -d "$PIPE/$d" ]; then
    echo "         MISSING: $PIPE/$d"
    missing=1
  else
    echo "         OK   $d ($(ls "$PIPE/$d"/*.R 2>/dev/null | wc -l | tr -d ' ') scripts)"
  fi
done
if [ "$missing" = 1 ]; then
  echo
  echo "         R06 sources plume_scripts/P01-P06 and would fail after stage 1"
  echo "         had already run for hours. Restore them from the repo:"
  echo "           cd ~/Downloads/Suncor && git clone https://github.com/pdez90/AirToxics_Colorado.git"
  echo "           cp -R AirToxics_Colorado/plume_scripts AirToxics_Colorado/hrrr_scripts $PIPE/"
  exit 1
fi
unset SKIP_PLUMES

# The methane CSVs moved out of Toxics_EST; resolve them here so a wrong path
# is caught in the pre-flight rather than 30 seconds into stage 1.
METH=""
for c in "$HOME/Downloads/MethaneData" "/Users/priyanka/Toxics_EST/MethaneData" "$BASE/MethaneData"; do
  if [ -d "$c" ]; then METH="$c"; break; fi
done
if [ -z "$METH" ]; then
  echo "         MISSING: MethaneData folder (looked in ~/Downloads, Toxics_EST, $BASE)"
  echo "                  set METHANE_DIR=/path/to/MethaneData and re-run"
  exit 1
fi
NMETH=$(find "$METH" -name "*_Methane.csv" 2>/dev/null | wc -l | tr -d ' ')
echo "         OK   MethaneData: $METH ($NMETH CSVs)"
if [ "$NMETH" -lt 290 ]; then
  echo "         STOP: expected at least 290 methane CSVs, found $NMETH"
  exit 1
fi
export METHANE_DIR="$METH"

echo "         installing splitr if needed (group I / Figure S4.8; not on CRAN)"
Rscript -e '
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes", repos = "https://cloud.r-project.org")
if (!requireNamespace("splitr", quietly = TRUE))
  remotes::install_github("rich-iannone/splitr")
cat("splitr available:", requireNamespace("splitr", quietly = TRUE), "\n")'   > "$LOGDIR/00a_splitr.log" 2>&1
if grep -q "splitr available: TRUE" "$LOGDIR/00a_splitr.log"; then
  echo "         OK   splitr"
else
  echo "         WARN splitr not available - group I (Figure S4.8) will fail."
  echo "              See $LOGDIR/00a_splitr.log. Everything else still runs."
fi

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
run_stage "stage 3: MAKE_FIGURES.R (all groups, incl. HYSPLIT)  ~13-16 h" \
          "$LOGDIR/03_figures.log" \
          env R_MAX_VSIZE=100Gb Rscript "$PIPE/MAKE_FIGURES.R"

# MAKE_FIGURES exits 0 even when an individual group fails, so read the per-
# group logs rather than the exit code. This is the trap that made the
# September run report "failed: 0" while eleven groups had in fact stopped.
echo
echo "-------- stage 3 per-group result --------"
for f in "$PIPE"/logs/FIG_*_console.txt; do
  g=$(basename "$f" | sed 's/FIG_//; s/_console.txt//')
  if grep -q "^  \[FAIL\] group $g" "$f" 2>/dev/null; then
    echo "         FAIL group $g"
    FAILED+=("fig_$g")
  fi
done
[ ${#FAILED[@]} -eq 0 ] && echo "         all groups completed"

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
