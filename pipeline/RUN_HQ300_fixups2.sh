#!/bin/bash
# ==============================================================
# RUN_HQ300_fixups2.sh   (2026-09-22, evening)
# The two groups that failed in the first fixups run, plus B if it also
# tripped over the same masking problem.
#
#   bash ~/Downloads/Suncor/rerun_pipeline/RUN_HQ300_fixups2.sh
#
#   P  script 47 called mean(apply(...)) on a matrix; terra/raster make both
#      S4 generics with no matrix method inside the figure driver -> base::
#   Q  M06 asserted nrow(groups) >= 15, i.e. the 17 groups of the submitted
#      analysis. The headquarters exclusion legitimately leaves 14, so the
#      guard now requires only that groups exist and reports the count.
#   B  script 09 has the same apply() problem (patched); this script re-runs B
#      only if its last run failed, so a successful B is not repeated.
# ==============================================================
BASE="$HOME/Downloads/Suncor"
PIPE="$BASE/rerun_pipeline"
LOG="$PIPE/logs/hq300_fixups2_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOG"; cd "$BASE" || exit 1
say() { echo "[$(date +%H:%M:%S)] $*" | tee -a "$LOG/FIXUPS2.log"; }
FAILED=""
fig() {
  local g="$1" t0=$(date +%s)
  say "START group $g"
  caffeinate -i env GROUPS="$g" R_MAX_VSIZE=100Gb Rscript "$PIPE/MAKE_FIGURES.R" > "$LOG/fig_$g.log" 2>&1
  local mins=$(( ($(date +%s) - t0) / 60 ))
  if grep -q "^  \[FAIL\] group $g" "$LOG/fig_$g.log"; then
    say "FAIL  group $g ($mins min)"
    tail -6 "$PIPE/logs/FIG_${g}_console.txt" | sed 's/^/        | /' | tee -a "$LOG/FIXUPS2.log"
    FAILED="$FAILED $g"
  else
    say "DONE  group $g ($mins min)"
  fi
}

for g in P Q; do fig "$g"; done
if grep -q "^  \[FAIL\] group B" "$PIPE/logs/FIG_B_console.txt" 2>/dev/null; then
  fig B
else
  say "group B already completed - not repeating it"
fi

say "=== SUMMARY ==="
if [ -n "$FAILED" ]; then say "still failing:$FAILED"; else say "all done"; fi
say "logs: $LOG"
