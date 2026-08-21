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
      # BUGFIX (2026-08-20): the fallback was REF$p99, but REF now holds the
      # CURRENT values, so on a clean from-raw run (no BACKUP) this printed
      # 4.6 / 11 in the OLD column - identical to the new column - making the
      # report say "no change" for exactly the two numbers that changed.
      p_old <- if (!is.null(old_m)) signif(stats::quantile(old_m[[poll]], .99, na.rm = TRUE), 3) else REF_SUBMITTED$p99[[poll]]
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
# BUGFIX (2026-08-20): unlist() flattens the 3 x 4 scale_factors table
# column-major, so this wrote the La Casa MEAN CONCENTRATIONS (0.53/0.71/0.42)
# into the manuscript-numbers CSV as if they were the ratios. Read by name.
.sf_str <- function(o) {
  if (is.null(o)) return(NA_character_)
  if (!all(c("pollutant", "ratio_all_over_mobilelike") %in% names(o))) return(NA_character_)
  paste(signif(as.numeric(o[["ratio_all_over_mobilelike"]])[
          match(c("benzene", "toluene", "xylene"), o[["pollutant"]])], 3), collapse = "/")
}
add_row("S2.5.2", "La Casa scaling factors (benzene/toluene/xylene)",
        ifelse(is.null(sf_old), "1.15/1.23/1.38 (ms)", .sf_str(sf_old)),
        .sf_str(sf_new))

# ---- 5. blocks + population (Section 3.3) ----
# BUGFIX (2026-08-20): blocks_summaries_clean.RData has no writer anywhere in
# the pipeline, so bl_new was always NULL and this row printed the hard-coded
# "1120 (ms)" as though it had been verified. The real file is script 18's.
.BLOCKS_FILE <- "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData"
bl_new <- load_first(file.path(BASE, .BLOCKS_FILE))
bl_old <- load_first(file.path(BACKUP, .BLOCKS_FILE))
add_row("S3.3", "census blocks with >=1 mobile point",
        ifelse(is.null(bl_old), NA, nrow(bl_old)),
        ifelse(is.null(bl_new), NA, nrow(bl_new)))

# BUGFIX (2026-08-20): the row above counts blocks with mobile COVERAGE, which
# is not the manuscript's block count. The published 1,668 blocks / 126,607
# residents are the COMMON blocks (AirToxScreen benzene + mobile benzene +
# population > 0) written by 20_census_block_level_health_risks.R. Read that
# file so the deliverable reports the number the manuscript actually cites.
.rc <- list.files(BASE, pattern = "risk.*common.*[.]csv$", ignore.case = TRUE,
                  full.names = TRUE)
if (length(.rc)) {
  .rcx <- try(utils::read.csv(.rc[1]), silent = TRUE)
  if (!inherits(.rcx, "try-error") && "n_blocks" %in% names(.rcx)) {
    add_row("S3.3", paste0("COMMON census blocks (", basename(.rc[1]), ")"),
            paste0(REF_SUBMITTED$n_blocks, " (as submitted)"), .rcx$n_blocks[1])
    if ("population" %in% names(.rcx)) {
      add_row("S3.3", "population in common blocks",
              paste0(REF_SUBMITTED$population, " (as submitted)"), .rcx$population[1])
    }
  }
} else {
  add_row("S3.3", "COMMON census blocks",
          paste0(REF_SUBMITTED$n_blocks, " (as submitted)"),
          "NOT FOUND - run R04b then script 20")
}

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

# BUGFIX (2026-08-21): the range was taken over EVERY row, including scenarios
# P08 marks `usable = FALSE` - intercepts beyond 2 sigma_y, where the inversion
# is governed by the Gaussian tail rather than by the measurement and returns
# 10^4-10^5 t/yr. This is the number the manuscript walk-through reads, so an
# unscreened maximum here becomes an unscreened maximum in the text. Screened
# now, with the excluded span reported rather than dropped silently.
res_str <- function(f) {
  if (!file.exists(f)) return(NA)
  r <- utils::read.csv(f)
  er <- grep("emission|rate|tons|Q_", names(r), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(er)) er <- names(r)[vapply(r, is.numeric, TRUE)][1]
  if (!"usable" %in% names(r))
    return(sprintf("n=%d; range %s-%s t/yr (UNSCREENED - file predates the well-posedness flag)",
                   nrow(r), signif(min(r[[er]], na.rm = TRUE), 3),
                   signif(max(r[[er]], na.rm = TRUE), 3)))
  ok <- as.logical(r$usable) %in% TRUE
  v  <- r[[er]][ok]
  out <- sprintf("n=%d well-posed of %d; range %s-%s t/yr", sum(ok), nrow(r),
                 signif(min(v, na.rm = TRUE), 3), signif(max(v, na.rm = TRUE), 3))
  if (any(!ok)) {
    b <- r[[er]][!ok]
    out <- paste0(out, sprintf("; %d ill-conditioned rows excluded (%s-%s t/yr, not emission estimates)",
                               sum(!ok), signif(min(b, na.rm = TRUE), 3),
                               signif(max(b, na.rm = TRUE), 3)))
  }
  out
}
# BUGFIX (2026-08-20): res_h2s.csv has no writer; P08 writes
# FinalFig/WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv.
.RES_REL <- file.path("FinalFig", "WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv")
add_row("S3.6", "H2S emission rates (t/yr, metric)",
        res_str(file.path(BACKUP, .RES_REL)), res_str(file.path(BASE, .RES_REL)))

# ---- write report ----
rep <- do.call(rbind, rows)
out_csv <- file.path(PIPE, "manuscript_numbers_old_vs_new.csv")
utils::write.csv(rep, out_csv, row.names = FALSE)
diag_msg("\nReport written: ", out_csv)
diag_msg("Walk the manuscript with this table: Abstract, 2.5.2, 2.5.3, 3.1, 3.3, 3.4, 3.6,")
diag_msg("Limitations, Discussion — plus SI sections S3-S6 and Figures 2-4, S4.1, S4.6-7.")
