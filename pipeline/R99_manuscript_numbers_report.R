# ==============================================================
# R99_manuscript_numbers_report.R
# Run LAST. Collates every manuscript-relevant number (old backup
# vs new corrected outputs) into one CSV + console report, so you
# can walk through the manuscript/SI and update values in one pass.
# Output: rerun_pipeline/manuscript_numbers_old_vs_new.csv
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R99: Manuscript numbers — old vs new")

rows <- list()
add_row <- function(section, item, old, new) {
  rows[[length(rows) + 1]] <<- data.frame(
    manuscript_section = section, item = item,
    old_value = as.character(old), new_value = as.character(new),
    stringsAsFactors = FALSE)
  diag_msg(sprintf("  %-28s %-46s old: %-14s new: %s", section, item, old, new))
}

load_first <- function(f) {
  if (!file.exists(f)) return(NULL)
  e <- new.env(); load(f, envir = e); get(ls(e)[1], envir = e)
}

# ---- 1. dataset size (Section 3.1: "~2 million mobile measurements") ----
old_m <- load_first(file.path(BACKUP, "mobile.RData"))
new_m <- load_first(file.path(BASE,   "mobile.RData"))
add_row("S3.1", "1-s mobile measurements (rows)",
        ifelse(is.null(old_m), NA, format(nrow(old_m), big.mark = ",")),
        ifelse(is.null(new_m), NA, format(nrow(new_m), big.mark = ",")))

# ---- 2. 99th percentile thresholds (Section 2.5.3) ----
if (!is.null(new_m)) {
  for (poll in names(REF$p99)) {
    if (poll %in% names(new_m)) {
      p_old <- if (!is.null(old_m)) signif(stats::quantile(old_m[[poll]], .99, na.rm = TRUE), 3) else REF$p99[[poll]]
      p_new <- signif(stats::quantile(new_m[[poll]], .99, na.rm = TRUE), 3)
      add_row("S2.5.3", paste0("99th percentile ", poll, " (ppb)"), p_old, p_new)
    }
  }
}

# ---- 3. below-MDL note (Section 3.1: >90% below MDL) ----
# (MDL values needed from CDPHE; report p50/p95 instead as a proxy)
if (!is.null(new_m)) {
  for (poll in c("Benzene_ppb", "Hydrogen_Sulfide_ppb", "Hydrogen_Cyanide_ppb")) {
    if (poll %in% names(new_m)) {
      add_row("S3.1", paste0("median (p50) ", poll, " (ppb)"),
              ifelse(is.null(old_m), NA, signif(stats::median(old_m[[poll]], na.rm = TRUE), 3)),
              signif(stats::median(new_m[[poll]], na.rm = TRUE), 3))
    }
  }
}

# ---- 4. scaling factors (Section 2.5.2 / Fig S4.1) ----
sf_new <- load_first(file.path(BASE, "lacasa_scaling_factors_option1_binweighted.RData"))
sf_old <- load_first(file.path(BACKUP, "lacasa_scaling_factors_option1_binweighted.RData"))
add_row("S2.5.2", "La Casa scaling factors (benzene/toluene/xylene)",
        ifelse(is.null(sf_old), "1.15/1.23/1.38 (ms)", paste(signif(as.numeric(unlist(sf_old)), 3), collapse = "/")),
        ifelse(is.null(sf_new), NA, paste(signif(as.numeric(unlist(sf_new)), 3), collapse = "/")))

# ---- 5. blocks + population (Section 3.3) ----
bl_new <- load_first(file.path(BASE, "blocks_summaries_clean.RData"))
bl_old <- load_first(file.path(BACKUP, "blocks_summaries_clean.RData"))
add_row("S3.3", "census blocks in analysis",
        ifelse(is.null(bl_old), "1120 (ms)", nrow(bl_old)),
        ifelse(is.null(bl_new), NA, nrow(bl_new)))

# ---- 6. hotspot groups (Sections 2.5.3.2, 3.4.2) ----
idx_new <- file.path(BASE, "MASTER_hotspot_group_index.csv")
idx_old <- file.path(BACKUP, "MASTER_hotspot_group_index.csv")
gcount <- function(f) {
  if (!file.exists(f)) return(NA)
  x <- utils::read.csv(f)
  gc <- grep("group", names(x), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(gc)) nrow(x) else length(unique(x[[gc]]))
}
add_row("S3.4.2", "persistent multi-pollutant hotspot groups",
        ifelse(is.na(gcount(idx_old)), "17 (ms)", gcount(idx_old)), gcount(idx_new))

# ---- 7. plume funnel + emission rates (Section 3.6) ----
fun_new <- file.path(BASE, "WWTP_H2S_plume_step_counts.csv")
fun_old <- file.path(BACKUP, "WWTP_H2S_plume_step_counts.csv")
fun_str <- function(f) {
  if (!file.exists(f)) return(NA)
  x <- utils::read.csv(f); paste(apply(x, 1, paste, collapse = ":"), collapse = " | ")
}
add_row("S3.6", "plume filtering funnel", fun_str(fun_old), fun_str(fun_new))

res_str <- function(f) {
  if (!file.exists(f)) return(NA)
  r <- utils::read.csv(f)
  er <- grep("emission|rate|tons|Q_", names(r), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(er)) er <- names(r)[vapply(r, is.numeric, TRUE)][1]
  sprintf("n=%d; range %s-%s t/yr", nrow(r),
          signif(min(r[[er]], na.rm = TRUE), 3), signif(max(r[[er]], na.rm = TRUE), 3))
}
add_row("S3.6", "H2S emission rates", res_str(file.path(BACKUP, "res_h2s.csv")),
        res_str(file.path(BASE, "res_h2s.csv")))

# ---- write report ----
rep <- do.call(rbind, rows)
out_csv <- file.path(PIPE, "manuscript_numbers_old_vs_new.csv")
utils::write.csv(rep, out_csv, row.names = FALSE)
diag_msg("\nReport written: ", out_csv)
diag_msg("Walk the manuscript with this table: Abstract, 2.5.2, 2.5.3, 3.1, 3.3, 3.4, 3.6,")
diag_msg("Limitations, Discussion — plus SI sections S3-S6 and Figures 2-4, S4.1, S4.6-7.")
