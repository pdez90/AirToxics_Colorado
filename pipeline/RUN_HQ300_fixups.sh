#!/bin/bash
# ==============================================================
# RUN_HQ300_fixups.sh   (2026-09-22)
# Re-runs the eleven figure groups that failed in the 13:08 Part A/B run.
# MAKE_FIGURES.R exits 0 even when a group fails, so the driver reported
# "failed: 0" while eleven groups had in fact stopped; this script reads each
# group's own log and reports the truth.
#
#   bash ~/Downloads/Suncor/rerun_pipeline/RUN_HQ300_fixups.sh
#
# What was wrong, and what changed
#   N -> H, P, Q   group N ran BEFORE group G, so the TRI-joined index it
#                  needs did not exist yet; H, P and Q then failed for want of
#                  MASTER_hotspot_group_index.csv. G has since run, so the
#                  order here is N, then H, P, Q.
#   Z, Y           a package attached by the figure driver masks base::Filter;
#                  it returned a function, so Figure 3's six-panel assembly and
#                  the La Casa scatter panel died AFTER their real outputs were
#                  written. Scripts 26 and 39 now subset explicitly.
#   J2             terra/raster make apply() an S4 generic with no data.frame
#                  method; script 69 now calls base::apply.
#   J              the static route JPEG (a supplied image, not a pipeline
#                  output) was lost with the rest of the folder; script 37 now
#                  writes the animation panel alone when it is missing.
#   B              needs the openairmaps package (installed below).
#   A, L           R ran out of vector memory. They run last, one at a time,
#                  with a raised limit. Close other applications first.
# ==============================================================
BASE="$HOME/Downloads/Suncor"
PIPE="$BASE/rerun_pipeline"
LOG="$PIPE/logs/hq300_fixups_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOG"; cd "$BASE" || exit 1
say() { echo "[$(date +%H:%M:%S)] $*" | tee -a "$LOG/FIXUPS.log"; }
FAILED=""

fig() {   # fig <group>  - trusts the group's own log, not the exit code
  local g="$1" t0=$(date +%s)
  say "START group $g"
  caffeinate -i env GROUPS="$g" R_MAX_VSIZE=100Gb Rscript "$PIPE/MAKE_FIGURES.R" > "$LOG/fig_$g.log" 2>&1
  local mins=$(( ($(date +%s) - t0) / 60 ))
  if grep -q "^  \[FAIL\] group $g" "$LOG/fig_$g.log"; then
    say "FAIL  group $g ($mins min) - see $PIPE/logs/FIG_${g}_console.txt"
    tail -6 "$PIPE/logs/FIG_${g}_console.txt" | sed 's/^/        | /' | tee -a "$LOG/FIXUPS.log"
    FAILED="$FAILED $g"
  else
    say "DONE  group $g ($mins min)"
  fi
}

say "installing openairmaps if missing (group B)"
Rscript -e 'if (!requireNamespace("openairmaps", quietly = TRUE))
              install.packages("openairmaps", repos = "https://cloud.r-project.org")' \
  >> "$LOG/packages.log" 2>&1

# 1) index chain, in dependency order, then the groups that read it
for g in N H P Q; do fig "$g"; done
# 2) the ones that only needed the masking fixes
for g in Z Y J2 J B; do fig "$g"; done
# 3) memory-hungry, last and alone
for g in A L; do fig "$g"; done

say "=== SUMMARY ==="
if [ -n "$FAILED" ]; then say "still failing:$FAILED"; else say "all eleven groups completed"; fi
say "logs: $LOG"
