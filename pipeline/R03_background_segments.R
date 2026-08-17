# ==============================================================
# R03_background_segments.R
# Re-runs background correction (rolling lowest-20th-percentile)
# and the 500 m segment aggregation feeding Figure 2, then checks
# the manuscript's segment-level concentration ranges.
#
# Sources:
#   R_scripts/10_calculating_background_air_pollution_concentrations_rolling_.R
#   R_scripts/11_correcting_for_background.R
#   R_scripts/12_finding_500_meter_road_segment.R
#   R_scripts/13_suncor_terminal_calculating_aggregate_stats_for_each_500_m_s.R
#   R_scripts/14_aggregate_stats_for_each_500_m_segment.R
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R03: Background correction + 500 m segments")

t0 <- Sys.time()
source(file.path(BASE, "R_scripts", "10_calculating_background_air_pollution_concentrations_rolling_.R"))
source(file.path(BASE, "R_scripts", "11_correcting_for_background.R"))
source(file.path(BASE, "R_scripts", "12_finding_500_meter_road_segment.R"))
source(file.path(BASE, "R_scripts", "13_suncor_terminal_calculating_aggregate_stats_for_each_500_m_s.R"))
source(file.path(BASE, "R_scripts", "14_aggregate_stats_for_each_500_m_segment.R"))
diag_msg("Section scripts completed in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# ----------------------------------------------------------------
# DIAG 1: background-corrected dataset vs old
# ----------------------------------------------------------------
diag_section("R03-DIAG 1: background-corrected dataset")
diag_compare_rdata_rows("bgcorrected_out_merge.RData", "df")

e_new <- new.env(); load(file.path(BASE, "bgcorrected_out_merge.RData"), envir = e_new)
bg <- get(ls(e_new)[1], envir = e_new)
diag_df_summary(bg, "bgcorrected_out_merge (new)")

# Background correction sanity: medians of corrected data should sit near 0
for (cc in intersect(c("Benzene_ppb", "Hydrogen_Sulfide_ppb", "Hydrogen_Cyanide_ppb"), names(bg))) {
  md <- stats::median(bg[[cc]], na.rm = TRUE)
  diag_msg(sprintf("  [SANITY] median background-corrected %-22s = %8.4f ppb (expect near 0)", cc, md))
}

# ----------------------------------------------------------------
# DIAG 2: raw 99th percentiles (drive the hotspot thresholds AND
# Section 2.5.3 text). Compare against the manuscript values.
# ----------------------------------------------------------------
diag_section("R03-DIAG 2: 99th percentiles vs manuscript (Section 2.5.3)")
e_m <- new.env(); load(file.path(BASE, "mobile.RData"), envir = e_m)
mob <- get(ls(e_m)[1], envir = e_m)
for (poll in names(REF$p99)) {
  if (poll %in% names(mob)) {
    p99_new <- as.numeric(stats::quantile(mob[[poll]], 0.99, na.rm = TRUE))
    diag_check_value(paste0("p99 ", poll), p99_new, REF$p99[[poll]], tol_pct = 10)
  }
}
diag_msg("  NOTE: any flagged p99 changes must be updated at manuscript lines ~401-405.")

# ----------------------------------------------------------------
# DIAG 3: segment-level ranges feeding Figure 2 text (Section 3.3)
# ----------------------------------------------------------------
diag_section("R03-DIAG 3: 500 m segment summaries (Figure 2 text ranges)")
segf <- file.path(BASE, "segment500_summaries_clean.RData")
if (file.exists(segf)) {
  e_s <- new.env(); load(segf, envir = e_s)
  segs <- get(ls(e_s)[1], envir = e_s)
  diag_msg("  Loaded segment summaries: ", paste(ls(e_s), collapse = ", "))
  if (is.data.frame(segs)) {
    num_cols <- names(segs)[vapply(segs, is.numeric, TRUE)]
    for (cc in num_cols[grepl("ppb|median|Benzene|Toluene|Xylene|Trimethyl|Sulfide|Cyanide",
                               num_cols, ignore.case = TRUE)]) {
      diag_msg(sprintf("  [RANGE] %-40s min: %-9s p99: %-9s max: %s",
                       cc, signif(min(segs[[cc]], na.rm = TRUE), 3),
                       signif(stats::quantile(segs[[cc]], 0.99, na.rm = TRUE), 3),
                       signif(max(segs[[cc]], na.rm = TRUE), 3)))
    }
  }
  diag_msg("  Manuscript ranges to verify against these (Section 3.3):")
  diag_msg("    benzene ~0-0.55 ppb; toluene up to ~1.2 ppb; H2S ~-1 to 2 ppb; HCN ~0.5-1.0 (max >=1.2) ppb")
} else diag_msg("  [WARN] segment summaries file not found: ", segf)

diag_msg("\nR03 complete. Regenerate Figure 2 with R_scripts/15_maps_for_500_m_segment.R when satisfied.")
