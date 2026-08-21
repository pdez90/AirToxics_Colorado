# ==============================================================
# R04_scaling_census_risk.R
# Re-runs La Casa temporal scaling factors, census-block stats,
# and the benzene cancer-risk comparison vs AirToxScreen.
# These feed: Fig S4.1 (scaling), Fig S4.6-7, and THE headline
# numbers (0.183-0.650 cases; 2.4x AirToxScreen).
#
# Sources:
#   R_scripts/17_suncor_terminal_calculating_scaling_factors_to_calculate_dai.R
#   R_scripts/18_suncor_terminal_calculating_census_block_level_stats.R
#   R_scripts/20_census_block_level_health_risks.R
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R04: Scaling factors + census blocks + benzene risk")

# ----------------------------------------------------------------
# PREFLIGHT: script 18 loads mobile_corrected.RData from
# /Users/priyanka/Downloads/ (NOT the Suncor subfolder — original
# code path). If a stale pre-fix copy sits there, script 18 would
# silently use OLD data. Sync the fresh file to that path first.
# ----------------------------------------------------------------
src_mc <- file.path(BASE, "mobile_corrected.RData")
dst_mc <- "/Users/priyanka/Downloads/mobile_corrected.RData"
if (file.exists(src_mc)) {
  ok <- file.copy(src_mc, dst_mc, overwrite = TRUE, copy.date = TRUE)
  diag_msg("  [PREFLIGHT] synced fresh mobile_corrected.RData to Downloads root ",
           "(script 18 loads it from there): ", ok)
} else {
  stop("mobile_corrected.RData not found in ", BASE, " — run R03 first.")
}

t0 <- Sys.time()
source(file.path(BASE, "R_scripts", "17_suncor_terminal_calculating_scaling_factors_to_calculate_dai.R"))
source(file.path(BASE, "R_scripts", "18_suncor_terminal_calculating_census_block_level_stats.R"))
# NOTE: script 20 (benzene risk) requires block_sf_risk, which is constructed
# by R04b_build_block_sf_risk.R — run R04b after this script (RUN_ALL does).
diag_msg("Section scripts completed in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# ----------------------------------------------------------------
# DIAG 1: temporal scaling factors vs manuscript (1.15 / 1.23 / 1.38)
# NOTE: delays shift data by seconds; hour-of-day weights should be
# essentially unchanged, so scaling factors should move very little.
# A big change here signals a pipeline problem, not a delay effect.
# ----------------------------------------------------------------
diag_section("R04-DIAG 1: La Casa temporal scaling factors")
sf_file <- file.path(BASE, "lacasa_scaling_factors_option1_binweighted.RData")
if (!file.exists(sf_file)) sf_file <- file.path(BASE, "lacasa_scaling_factors_option1.RData")
if (file.exists(sf_file)) {
  e_sf <- new.env(); load(sf_file, envir = e_sf)
  diag_msg("  Objects in ", basename(sf_file), ": ", paste(ls(e_sf), collapse = ", "))
  sf_obj <- get(ls(e_sf)[1], envir = e_sf)
  print(sf_obj); capture.output(print(sf_obj), file = .DIAG_LOG, append = TRUE)
  # BUGFIX (2026-08-20): this was `as.numeric(unlist(sf_obj))` then drop NAs.
  # scale_factors is a 3 x 4 table (pollutant, lc_mean_all, lc_mean_mobilelike,
  # ratio_all_over_mobilelike) and unlist() flattens COLUMN-major, so
  # sf_num[1:3] were the La Casa mean concentrations, never the ratios - the
  # check therefore reported "CHANGED beyond tolerance" on every run.
  # Read by name, exactly as script 18's get_ratio() does.
  .get_ratio <- function(pol) {
    if (!all(c("pollutant", "ratio_all_over_mobilelike") %in% names(sf_obj))) return(NA_real_)
    r <- sf_obj[["ratio_all_over_mobilelike"]][match(pol, sf_obj[["pollutant"]])]
    if (length(r) != 1) NA_real_ else as.numeric(r)
  }
  for (.pol in c("benzene", "toluene", "xylene")) {
    diag_check_value(sprintf("scaling factor (%s, ms %.2f)", .pol, REF$scaling[[.pol]]),
                     .get_ratio(.pol), REF$scaling[[.pol]], tol_pct = 5)
  }
} else diag_msg("  [WARN] scaling factor file not found.")

# ----------------------------------------------------------------
# DIAG 2: census-block coverage vs manuscript (1,120 blocks; 83,828 residents)
# ----------------------------------------------------------------
diag_section("R04-DIAG 2: census-block coverage")
# BUGFIX (2026-08-20): nothing in the pipeline writes blocks_summaries_clean
# .RData - script 18 writes censusblocks_suncor_terminal_BINWEIGHTED_AB.RData
# (objects block_sf, block_dt). file.exists() was therefore always FALSE and
# the block-count and population checks never executed. Point at the real
# file, preferring block_dt.
.BLOCKS_FILE <- "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData"
diag_compare_rdata_rows(.BLOCKS_FILE, "block_dt")
bl_file <- file.path(BASE, .BLOCKS_FILE)
if (file.exists(bl_file)) {
  e_b <- new.env(); load(bl_file, envir = e_b)
  bl <- get(if ("block_dt" %in% ls(e_b)) "block_dt" else ls(e_b)[1], envir = e_b)
  if (is.data.frame(bl)) {
    # blocks with both AirToxScreen + mobile benzene, if identifiable
    diag_msg("  columns: ", paste(head(names(bl), 25), collapse = ", "))
    # BUGFIX (2026-08-20): this compared nrow(block_dt) against REF$n_blocks.
    # They are different quantities. Script 18 builds block_dt as EVERY block
    # containing at least one mobile point (`block_dt[!is.na(n_points)]`), with
    # no population and no AirToxScreen requirement, so nrow(block_dt) is
    # necessarily larger than the manuscript's 1,668 - which is the COMMON
    # block count from 20_census_block_level_health_risks.R (finite
    # Population_airtox > 0 AND finite benzene_ppb_airtox AND finite mobile
    # benzene). At tol_pct = 5 the old comparison flagged a regression on every
    # run. Report the coverage count here; the 1,668 check belongs to R04b/20.
    diag_msg(sprintf("  [COUNT] census blocks with >=1 mobile point: %s (no benchmark - ",
                     format(nrow(bl), big.mark = ",")),
             "this is NOT the manuscript's ", REF$n_blocks,
             " common blocks, which are checked from script 20's ",
             "results_risk_common after R04b.)")
    pop_col <- grep("pop", names(bl), ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(pop_col)) {
      diag_check_value(paste0("total population (", pop_col, ")"),
                       sum(bl[[pop_col]], na.rm = TRUE), REF$population, tol_pct = 5)
    } else {
      # script 18 drops POP20 at line ~70, so the population total has to come
      # from the block-risk object built by R04b. Say so rather than passing
      # silently, which is what this branch used to do.
      diag_msg("  [CHECK-SKIP] total population: no population column in ", .BLOCKS_FILE,
               " (script 18 drops POP20) - verify against R04b's block_sf_risk instead. ",
               "Expected: ", REF$population)
    }
  }
}

# ----------------------------------------------------------------
# DIAG 3: benzene cancer-risk numbers vs manuscript
# The risk script (20_*) computes these; recompute here from block data
# if the objects are in the workspace, otherwise instruct manual check.
# ----------------------------------------------------------------
diag_section("R04-DIAG 3: benzene cancer risk -> computed by R04b (next stage)")
diag_msg("  Manuscript (old delays): AirToxScreen 0.077-0.275 cases; mobile 0.183-0.650 cases; ratio ~2.4x")
risk_objs <- ls()[grepl("risk", ls(), ignore.case = TRUE)]
if (length(risk_objs)) {
  for (ro in risk_objs) {
    diag_msg("  found object: ", ro)
    out <- try(capture.output(print(get(ro))), silent = TRUE)
    if (!inherits(out, "try-error")) {
      for (l in head(out, 20)) diag_msg("    ", l)
    }
  }
} else {
  diag_msg("  [ACTION] risk objects not auto-detected in workspace; check the printed output")
  diag_msg("           of 20_census_block_level_health_risks.R above and compare to:")
  diag_msg("           0.183-0.650 (mobile), 0.077-0.275 (AirToxScreen), ratio 2.4x.")
}
diag_msg("  Any change here propagates to: Abstract, Section 3.3, Discussion, and the response letter (R2.2).")

diag_msg("\nR04 complete.")
