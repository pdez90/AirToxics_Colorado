# ==============================================================
# HQ300_before_after.R  (2026-09-22)
# What the 300 m CDPHE-headquarters exclusion changed.
#
# Compares the re-run (03_checks_flags.R section 3c, HQ_RADIUS_M = 300)
# against the published manuscript baseline:
#   - baseline_manuscript/*.rds  = the Shiny app data built on 2026-08-24
#     from the run the manuscript reports (verified against Table S3.2,
#     Table S5.1 and Section 2.5.3.2 at the time)
#   - hard-coded Section 2.5.3.2 counts and persistence thresholds
# and writes HQ300_before_after.txt next to this script's outputs.
#
# Run AFTER R01 -> R02 -> R03 -> R05 (RUN_HQ300.sh does this).
# ==============================================================
suppressPackageStartupMessages({ library(data.table) })
BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
BL   <- file.path(BASE, "baseline_manuscript")
OUT  <- file.path(BASE, "HQ300_before_after.txt")
HQ   <- c(lat = 39.785189, lon = -105.104411)
sink(OUT, split = TRUE)
on.exit(sink(), add = TRUE)

hav <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
       cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371008.8 * asin(pmin(1, sqrt(a)))
}
rule <- function(t) cat("\n", strrep("=", 70), "\n", t, "\n", strrep("=", 70), "\n", sep = "")
ok   <- 0L; bad <- 0L
chk  <- function(label, cond) {
  cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
  if (isTRUE(cond)) ok <<- ok + 1L else bad <<- bad + 1L
}

cat("HQ exclusion before/after report —", format(Sys.time()), "\n")
cat("HQ point:", HQ["lat"], HQ["lon"], "| radius 300 m\n")

# ------------------------------------------------------------------
rule("1. What was removed (written by 03_checks_flags.R section 3c)")
hq_tab <- file.path(BASE, "TABLE_hq_exclusion.csv")
if (file.exists(hq_tab)) {
  x <- fread(hq_tab)
  x[, pct := round(100 * n_removed / n_before, 2)]
  print(x[, .(pollutant, n_before, n_removed, pct)])
} else cat("  TABLE_hq_exclusion.csv not found — did R01 run with the patched 03?\n")

# ------------------------------------------------------------------
rule("2. Analysis set: campaign statistics before vs after")
load(file.path(BASE, "mobile_wswd.RData"))           # -> out
d <- as.data.table(out); rm(out); invisible(gc())
d <- d[is.finite(Latitude) & is.finite(Longitude) &
       Site != "Goodrich Corporation (Collins Aerospace)"]
dist_hq <- hav(d$Latitude, d$Longitude, HQ["lat"], HQ["lon"])
chk("no analysis-set row lies within 300 m of HQ", !any(dist_hq <= 300, na.rm = TRUE))
cat(sprintf("  nearest remaining row to HQ: %.0f m\n", min(dist_hq, na.rm = TRUE)))

POLLS <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb",
           Trimethylbenzene = "Trimethylbenzene_ppb", Xylene = "Xylene_ppb",
           H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")
new <- rbindlist(lapply(names(POLLS), function(pn) {
  v <- d[[POLLS[[pn]]]]; v <- v[is.finite(v)]
  data.table(pollutant = pn, n = length(v), median = round(median(v), 3),
             p95 = round(quantile(v, .95, names = FALSE), 3),
             p99 = round(quantile(v, .99, names = FALSE), 3), max = round(max(v), 1))
}))
f_old <- file.path(BL, "summary_stats.rds")
if (file.exists(f_old)) {
  old <- as.data.table(readRDS(f_old))[, .(pollutant, n_old = n, median_old = median,
                                           p95_old = p95, p99_old = p99, max_old = max)]
  cmp <- merge(old, new, by = "pollutant", sort = FALSE)
  cmp[, n_dropped := n_old - n]
  print(cmp[, .(pollutant, n_old, n, n_dropped, median_old, median, p99_old, p99, max_old, max)])
  cat("\n  p99 is the hotspot and source-probability event threshold. A change here\n",
      " moves every high-concentration event set downstream (Figures 3-4, S5).\n", sep = "")
} else print(new)
cat(sprintf("\n  sampling days in the analysis set: %d (manuscript: 203)\n",
            uniqueN(as.Date(d$date))))

# ------------------------------------------------------------------
rule("3. Hotspot clustering (Section 2.5.3.2)")
ms_initial <- c(benzene = 416, toluene = 423, trimethylbenzene = 483, xylene = 465,
                hydrogen_sulfide = 602, hydrogen_cyanide = 324)            # total 2,713
ms_persist <- c(benzene = 35, toluene = 33, trimethylbenzene = 32, xylene = 35,
                hydrogen_sulfide = 58, hydrogen_cyanide = 23)              # total 216
rows_in <- function(pat) {
  f <- list.files(BASE, pattern = pat, full.names = TRUE)
  setNames(vapply(f, function(z) nrow(fread(z)), integer(1)),
           sub(pat, "\\1", basename(f)))
}
ini <- rows_in("^cent_out_(.*)_all[.]csv$");        ini <- ini[names(ini) != "methane"]
per <- rows_in("^cent_out_(.*)_persistent[.]csv$"); per <- per[names(per) != "methane"]
cl <- data.table(pollutant = names(ms_initial),
                 initial_ms = ms_initial, initial_new = unname(ini[names(ms_initial)]),
                 persistent_ms = ms_persist, persistent_new = unname(per[names(ms_initial)]))
print(cl)
cat(sprintf("  totals: initial %s -> %s | persistent %s -> %s\n",
            sum(cl$initial_ms), sum(cl$initial_new, na.rm = TRUE),
            sum(cl$persistent_ms), sum(cl$persistent_new, na.rm = TRUE)))
th <- file.path(BASE, "hotspot_thresholds_summary.csv")
if (file.exists(th)) {
  cat("\n  persistence thresholds (manuscript: benzene 63.5/12d, toluene 44.8/8d,\n",
      "  TMB 39.4/6d, xylene 49/8d, H2S 39/13d, HCN 24.4/4d):\n", sep = "")
  print(fread(th))
}

# ------------------------------------------------------------------
rule("4. Persistent multi-pollutant groups: manuscript (17) vs re-run")
f_new <- file.path(BASE, "super_hotspots_3plus_persistent.csv")
f_bl  <- file.path(BL, "hotspots.rds")
if (file.exists(f_new) && file.exists(f_bl)) {
  g_new <- fread(f_new)
  g_old <- as.data.table(readRDS(f_bl)$groups)
  cat(sprintf("  groups persistent in >=3 pollutants: manuscript %d -> re-run %d\n",
              nrow(g_old), nrow(g_new)))
  # match each manuscript group to the nearest re-run group
  m <- rbindlist(lapply(seq_len(nrow(g_old)), function(i) {
    dd <- hav(g_old$Latitude[i], g_old$Longitude[i], g_new$Latitude, g_new$Longitude)
    j  <- which.min(dd)
    data.table(ms_group = g_old$group_id[i], ms_pollutants = g_old$pollutants[i],
               ms_max_days = g_old$max_n_days[i],
               dist_to_HQ_m = round(hav(g_old$Latitude[i], g_old$Longitude[i], HQ["lat"], HQ["lon"])),
               nearest_new_group = g_new$group_id[j], match_dist_m = round(dd[j]),
               new_n_pollutants = g_new$n_pollutants[j], new_max_days = g_new$max_n_days[j])
  }))
  m[, status := fifelse(match_dist_m <= 300, "retained (<=300 m)", "NOT FOUND")]
  print(m)
  cat(sprintf("\n  manuscript groups recovered within 300 m: %d of %d\n",
              sum(m$match_dist_m <= 300), nrow(m)))
  g9 <- m[ms_group == 9]
  if (nrow(g9))
    cat(sprintf("  Group 9 (%d m from HQ): nearest re-run group is %.0f m away -> %s\n",
                g9$dist_to_HQ_m, g9$match_dist_m,
                if (g9$match_dist_m > 300) "REMOVED, as expected (garage artefact)"
                else "STILL PRESENT — investigate"))
  # re-run groups with no manuscript counterpart
  nn <- vapply(seq_len(nrow(g_new)), function(j)
    min(hav(g_new$Latitude[j], g_new$Longitude[j], g_old$Latitude, g_old$Longitude)), numeric(1))
  new_only <- g_new[nn > 300]
  cat(sprintf("  re-run groups with no manuscript group within 300 m: %d\n", nrow(new_only)))
  if (nrow(new_only)) print(new_only[, intersect(c("group_id","Latitude","Longitude","n_pollutants",
                                                  "max_n_days","pollutants"), names(new_only)), with = FALSE])
  cat("\n  NOTE: group_id values are assigned by the clustering and can renumber when\n",
      "  the input changes; every 'Group N' reference in the text must be re-mapped\n",
      "  using the ms_group -> nearest_new_group column above.\n", sep = "")
} else cat("  missing", if (!file.exists(f_new)) f_new, if (!file.exists(f_bl)) f_bl, "\n")

rule(sprintf("Checks: %d passed, %d failed", ok, bad))
cat("Report written to", OUT, "\n")
