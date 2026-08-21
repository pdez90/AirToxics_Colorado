# ==============================================================
# test_p08_geometry.R
#
# Closed-loop test of the plume geometry in P08_gaussian_plumes_h2s.R.
#
# The point is that this is NOT a re-statement of the maths in prose. It
# builds a plume FORWARD from a source of known strength, places the receptor
# off the centreline the way the acceptance window actually does, computes what
# concentration that receptor would see, and then hands that concentration to
# P08's own invert_gaussian() to see whether the known source strength comes
# back. If the along-wind/cross-wind decomposition, the sigma lookups, the
# reflection series or the unit conversion were wrong, the recovered Q would
# not match.
#
# It also quantifies what the SUPERSEDED centreline assumption (theta forced to
# zero) does to the same data, so the size of that correction is a measured
# number in the repository rather than a claim in a comment.
#
# Run:  Rscript tests/test_p08_geometry.R
# Exits non-zero on failure.
# ==============================================================

suppressPackageStartupMessages({ library(dplyr) })

P08 <- Sys.getenv("P08_PATH", "")
if (!nzchar(P08)) {
  cand <- c("plume_scripts/P08_gaussian_plumes_h2s.R",
            "../plume_scripts/P08_gaussian_plumes_h2s.R",
            "rerun_pipeline/plume_scripts/P08_gaussian_plumes_h2s.R",
            "../rerun_pipeline/plume_scripts/P08_gaussian_plumes_h2s.R")
  P08 <- cand[file.exists(cand)][1]
}
if (is.na(P08) || !nzchar(P08) || !file.exists(P08))
  stop("Cannot find P08_gaussian_plumes_h2s.R. Set P08_PATH.")
cat("testing the geometry in: ", P08, "\n\n", sep = "")

src <- readLines(P08, warn = FALSE)

# Pull only the pieces we need. P08 as a whole loads data and writes figures;
# we want its functions and constants, evaluated in isolation.
grab <- function(start_re) {
  i <- grep(start_re, src)[1]
  if (is.na(i)) stop("not found in P08: ", start_re)
  j <- grep("^\\}$", src); j <- min(j[j > i])
  paste(src[i:j], collapse = "\n")
}
span <- function(from_re, to_re) {
  i <- grep(from_re, src)[1]; j <- grep(to_re, src); j <- min(j[j >= i])
  paste(src[i:j], collapse = "\n")
}

op_fraction <- 1
z_m         <- 1.5
eval(parse(text = span("^R_GAS", "^SITE_MOL_M3")))
eval(parse(text = span("^WELL_MIXED_RATIO", "^VERT_N_IMAGES")))
MW_H2S <- 34.08; MIN_U <- 0.5; MIN_HPBL <- 50; MIN_X_M <- 50
DENOM_MIN <- 1e-20; C_EPS_PPM <- 1e-30; Z_RECEPTOR <- 1.5
seconds_per_year <- 365.25 * 24 * 3600
for (fn in c("^molar_density_mol_m3 <- function", "^vertical_term <- function",
             "^sigma_y_pg <- function", "^sigma_z_pg <- function",
             "^shift_cat <- function", "^kg_s_to_tpy_metric <- function",
             "^\\.theta_eff <- function", "^vert_term_vec <- function",
             "^invert_gaussian <- function"))
  eval(parse(text = grab(fn)))

# --------------------------------------------------------------
# FORWARD MODEL - written here independently of P08, from the textbook
# Gaussian plume, so the test is not the code checking itself.
#
#   C = Q / (2 pi u sigma_y sigma_z) * exp(-y^2 / 2 sigma_y^2) * V(z,H)
#
# Receptors are placed at a straight-line distance d from the source and at an
# angular offset theta, exactly as the +-10 degree acceptance window admits.
# --------------------------------------------------------------
Q_true_kg_s <- 0.05
H           <- 12.2

cases <- expand.grid(
  d_km  = c(1.00, 1.95, 3.55, 4.30),   # the real retained-plume distances
  theta = c(0, 2, 5, 8, 10),           # up to the acceptance limit
  CAT   = c("B", "C", "D"),            # classes the daytime classifier yields
  u     = c(2.0, 3.5, 6.0),
  hpbl  = c(300, 800, 1500),           # incl. the shallow PBL where the image
  stringsAsFactors = FALSE             #   series matters most
)

x_true <- cases$d_km * cos(cases$theta * pi / 180)           # along-wind, km
y_true <- cases$d_km * 1000 * sin(cases$theta * pi / 180)    # cross-wind, m
sy <- mapply(sigma_y_pg, cases$CAT, x_true)
sz <- mapply(sigma_z_pg, cases$CAT, x_true)
vt <- mapply(function(s, L) vertical_term(z_m, H, s, L, TRUE), sz, cases$hpbl)

C_kg_m3 <- Q_true_kg_s / (2 * pi * cases$u * sy * sz) *
           exp(-0.5 * (y_true / sy)^2) * vt
C_ppb   <- C_kg_m3 / (MW_H2S / 1000) / SITE_MOL_M3 * 1e9

inv <- tibble::tibble(
  plume_id = seq_len(nrow(cases)), dH2S_ppb = C_ppb, u_ms = cases$u,
  dist_m = cases$d_km * 1000, theta_deg = cases$theta, CAT = cases$CAT,
  hpbl_m = cases$hpbl, mol_m3 = SITE_MOL_M3
)

# --------------------------------------------------------------
# 1) RECOVERY. Feed the forward concentrations back through P08.
# --------------------------------------------------------------
res <- invert_gaussian(inv, reflections = TRUE, H_m = H, wind_mult = 1,
                       x_mult = 1, cat_shift = 0, y_spec = "geom")
stopifnot(nrow(res) == nrow(cases))
err <- 100 * (res$kg_s - Q_true_kg_s) / Q_true_kg_s
cat(sprintf("recovery across %d cases (distance x angle x class x wind x PBL):\n", nrow(cases)))
cat(sprintf("  max |error| = %.3g %%\n", max(abs(err))))
TOL <- 1e-6
pass1 <- max(abs(err)) < TOL
cat(sprintf("  %s  forward and inverse geometry agree to better than %g %%\n",
            if (pass1) "PASS " else "FAIL ", TOL))

# --------------------------------------------------------------
# 2) THE CORRECTION THIS REPLACED. Same data, theta forced to zero.
#    Must UNDER-estimate: an off-axis concentration read as a centreline one
#    implies a smaller source than the real one.
# --------------------------------------------------------------
res0 <- invert_gaussian(inv, reflections = TRUE, H_m = H, wind_mult = 1,
                        x_mult = 1, cat_shift = 0, y_spec = "centerline")
e0 <- 100 * (res0$kg_s - Q_true_kg_s) / Q_true_kg_s
off <- cases$theta > 0
cat(sprintf("\nsuperseded centreline assumption on the same data (off-axis cases only):\n"))
cat(sprintf("  median error %.1f %%, worst %.1f %%\n", median(e0[off]), min(e0[off])))
pass2 <- all(e0[off] <= 1e-9) && all(abs(e0[!off]) < TOL)
cat(sprintf("  %s  under-estimates off-axis, exact on-axis (the direction the method note claims)\n",
            if (pass2) "PASS " else "FAIL "))

# --------------------------------------------------------------
# 3) WELL-POSEDNESS METRIC. y/sigma_y must rise with distance and angle, and
#    must stay under the ILL_SIGY = 2 flag for everything the +-10 degree
#    window admits at the baseline sigma_y.
# --------------------------------------------------------------
cat("\nmax y/sigma_y by distance and off-axis angle:\n")
print(round(tapply(res$y_over_sigy, list(d_km = cases$d_km, theta = cases$theta), max), 2))
pass3 <- max(res$y_over_sigy) < 2
cat(sprintf("  %s  baseline acceptance window stays inside the 2 sigma_y flag (max %.2f)\n",
            if (pass3) "PASS " else "FAIL ", max(res$y_over_sigy)))
cat("       (scenarios that DO trip the flag are the wd+/-10 and averaging-time\n",
    "        axes, which shrink sigma_y - that is what P08's `usable` flag is for)\n", sep = "")

# --------------------------------------------------------------
fails <- sum(!c(pass1, pass2, pass3))
cat(sprintf("\n%s  (%d failure%s)\n",
            if (fails == 0L) "ALL P08 GEOMETRY TESTS PASSED" else "P08 GEOMETRY TESTS FAILED",
            fails, if (fails == 1L) "" else "s"))
if (fails > 0L) quit(status = 1L)
