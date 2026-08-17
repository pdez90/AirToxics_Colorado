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
  sf_num <- suppressWarnings(as.numeric(unlist(sf_obj)))
  sf_num <- sf_num[!is.na(sf_num)]
  if (length(sf_num) >= 3) {
    diag_check_value("scaling factor (benzene, ms 1.15)", sf_num[1], REF$scaling[["benzene"]], tol_pct = 5)
    diag_check_value("scaling factor (toluene, ms 1.23)", sf_num[2], REF$scaling[["toluene"]], tol_pct = 5)
    diag_check_value("scaling factor (xylene, ms 1.38)",  sf_num[3], REF$scaling[["xylene"]],  tol_pct = 5)
  }
} else diag_msg("  [WARN] scaling factor file not found.")

# ----------------------------------------------------------------
# DIAG 2: census-block coverage vs manuscript (1,120 blocks; 83,828 residents)
# ----------------------------------------------------------------
diag_section("R04-DIAG 2: census-block coverage")
diag_compare_rdata_rows("blocks_summaries_clean.RData", "blocks")
bl_file <- file.path(BASE, "blocks_summaries_clean.RData")
if (file.exists(bl_file)) {
  e_b <- new.env(); load(bl_file, envir = e_b)
  bl <- get(ls(e_b)[1], envir = e_b)
  if (is.data.frame(bl)) {
    # blocks with both AirToxScreen + mobile benzene, if identifiable
    diag_msg("  columns: ", paste(head(names(bl), 25), collapse = ", "))
    diag_check_value("n census blocks in summaries", nrow(bl), REF$n_blocks, tol_pct = 5)
    pop_col <- grep("pop", names(bl), ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(pop_col)) {
      diag_check_value(paste0("total population (", pop_col, ")"),
                       sum(bl[[pop_col]], na.rm = TRUE), REF$population, tol_pct = 5)
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
