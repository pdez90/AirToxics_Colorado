# ==============================================================
# FIG_A_corrplots_only.R
# Quick finisher for figure group A: the 52-min run of script 07
# failed only at its FINAL two figures (Corrplot_Suncor.jpeg /
# Corrplot_Terminal.jpeg) — everything before them was written.
# This regenerates just those two in ~2 min instead of rerunning
# the whole group. (GROUPS="A" MAKE_FIGURES.R remains the full,
# from-scratch option; script 07 itself is already patched.)
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("FIG-A finisher: Corrplot_Suncor + Corrplot_Terminal only")

suppressPackageStartupMessages({ library(dplyr); library(ggcorrplot) })

load(file.path(BASE, "mobile_wswd.RData"))
df <- out; rm(out)
diag_msg("  loaded mobile_wswd.RData: ", format(nrow(df), big.mark = ","), " rows")

# same in-place rename as script 07 (positions changed => select by NAME below)
df <- df %>% dplyr::rename(
  Benzene = Benzene_ppb, Toluene = Toluene_ppb,
  Trimethylbenzene = Trimethylbenzene_ppb, Xylene = Xylene_ppb,
  H2S = Hydrogen_Sulfide_ppb, HCN = Hydrogen_Cyanide_ppb)

vars <- c("Benzene","Toluene","Trimethylbenzene","Xylene","H2S","HCN",
          "ws","wd","Temperature_F","Pressure_mb","Relative_Humidity_percent")

make_corrplot <- function(site, outfile) {
  temp <- df[df$Site == site, ]
  temp <- temp %>% dplyr::select(dplyr::any_of(vars))
  # BUGFIX (2026-08-20): mobile_wswd.RData holds `out`, a data.table, and
  # dplyr::select preserves the class. `DT[, <logical vector>]` evaluates the
  # vector in `j` and returns THE VECTOR, not the columns, so cor() below fails
  # with "supply both 'x' and 'y' or a matrix-like 'x'". This is the identical
  # defect already fixed in 07_timeplot...R with an as.data.frame() coercion;
  # this file was missed.
  temp <- as.data.frame(temp)
  temp <- temp[, vapply(temp, is.numeric, logical(1)), drop = FALSE]
  stopifnot(is.data.frame(temp), ncol(temp) >= 2)
  diag_msg(sprintf("  %s: %s rows, %d numeric columns (%s)", site,
                   format(nrow(temp), big.mark = ","), ncol(temp),
                   paste(names(temp), collapse = ", ")))
  cm <- cor(temp, use = "pairwise.complete.obs")
  diag_msg(sprintf("    cor matrix %dx%d, NA cells: %d  |  benzene-toluene r = %.3f",
                   nrow(cm), ncol(cm), sum(is.na(cm)),
                   tryCatch(cm["Benzene", "Toluene"], error = function(e) NA)))
  jpeg(outfile, width = 5000, height = 5000, res = 600)
  print(ggcorrplot::ggcorrplot(as.matrix(cm), type = "lower", lab = TRUE))
  dev.off()
  diag_msg("    [SAVED] ", outfile)
}

make_corrplot("Suncor and Phillips 66 Terminal",
              file.path(BASE, "Corrplot_Suncor.jpeg"))
make_corrplot("Holly Energy Partners (Sinclair) Terminal",
              file.path(BASE, "Corrplot_Terminal.jpeg"))

diag_msg("\nDone — group A's last two figures regenerated. Full-group rerun optional.")
