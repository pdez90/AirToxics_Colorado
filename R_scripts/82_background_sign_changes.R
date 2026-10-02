# ==============================================================
# 82_background_sign_changes.R
# SI section S4.1.1 quotes how the background correction of
# 11_correcting_for_background.R (Equations 2-3) acts on the sign of the
# measurement, for benzene and H2S (the two species delivered with negative
# readings):
#   - share of negative readings that become positive, and of positive
#     readings that become negative (Equations 2-3 together);
#   - share of values whose run-median background is negative;
#   - number of values to which Equation 3 applies with a negative run median,
#     and how many positive measurements it turns negative.
# Counts are over values with a finite measurement, baseline and run median
# (the values the correction is defined for). H2S is one value per 5-s bin.
#   -> TABLE_background_sign_changes.csv
#   Rscript R_scripts/82_background_sign_changes.R
# ==============================================================
suppressPackageStartupMessages(library(data.table))
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
load(file.path(SUNCOR_BASE, "bgcorrected_out_merge.RData"))
df <- as.data.table(df)
if ("Site" %in% names(df)) df <- df[Site != "Goodrich Corporation (Collins Aerospace)"]

res <- rbindlist(lapply(c("Benzene", "H2S"), function(p) {
  obs <- df[[p]]; base <- df[[paste0("baseline_", p)]]; med <- df[[paste0("median_bg", p)]]; s <- df[[paste0("s", p)]]
  ok <- is.finite(obs) & is.finite(base) & is.finite(med) & is.finite(s)
  eq3 <- ok & base > obs & base > 0
  data.table(pollutant = p, n_values = sum(ok),
             pct_neg_to_pos = 100 * sum(ok & obs < 0 & s > 0) / sum(ok & obs < 0),
             pct_pos_to_neg = 100 * sum(ok & obs > 0 & s < 0) / sum(ok & obs > 0),
             pct_run_median_negative = 100 * mean(med[ok] < 0),
             n_eq3_negative_median = sum(eq3 & med < 0),
             pct_eq3_negative_median = 100 * sum(eq3 & med < 0) / sum(ok),
             n_eq3_pos_to_neg = sum(eq3 & med < 0 & obs > 0 & s < 0))
}))
fwrite(res, file.path(SUNCOR_BASE, "TABLE_background_sign_changes.csv"))
print(res)
message("-> ", file.path(SUNCOR_BASE, "TABLE_background_sign_changes.csv"))
