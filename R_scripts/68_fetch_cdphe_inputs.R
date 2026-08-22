# ==============================================================
# 68  Fetch the primary inputs straight from the CDPHE repository
#
# Makes the chain start at the public URL rather than at a local folder:
# the 20 quarterly data packets for the two study routes, and the ten
# HB21-1189 read-me documents that describe them, are downloaded from
#   https://www.colorado.gov/airquality/air_toxics_repo.aspx
# into Updated/ (packets) and Updated/readme/ (read-mes).
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/68_fetch_cdphe_inputs.R
#
# Default behaviour is to VERIFY, not overwrite: anything already present is
# downloaded to a temporary file, hashed, and compared with the local copy.
# Differences are reported and the local file is left alone. Set
#   SUNCOR_FETCH=1   to write missing files and replace differing ones
#   SUNCOR_FETCH=0   (default) to check only
#
# Revisions are pinned to the ones this study used: _r3 for 2023 Q1 - 2024 Q2,
# _r2 for 2024 Q3 - 2025 Q2. CDPHE re-posts packets under new revision
# suffixes, so a bare filename would not be reproducible. If a pinned file is
# gone from the repository the download fails loudly rather than silently
# picking up a different revision.
#
# The Goodrich route is not fetched: it is excluded from this study throughout.
# 2025 Q3 onward is outside the study period.
# ==============================================================
suppressPackageStartupMessages({library(tools)})

BASE  <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
DIR   <- file.path(BASE, "Updated")
RDIR  <- file.path(DIR, "readme")
WRITE <- identical(Sys.getenv("SUNCOR_FETCH", "0"), "1")
ROOT  <- "https://www.colorado.gov/airquality/air_toxics_repo.aspx?action=open&file="

dir.create(DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(RDIR, showWarnings = FALSE, recursive = TRUE)

# quarter -> revision suffix used by this study
REV <- c("2023_Q1" = "_r3", "2023_Q2" = "_r3", "2023_Q3" = "_r3", "2023_Q4" = "_r3",
         "2024_Q1" = "_r3", "2024_Q2" = "_r3", "2024_Q3" = "_r2", "2024_Q4" = "_r2",
         "2025_Q1" = "_r2", "2025_Q2" = "_r2")
ROUTE <- c("Commerce City-Suncor & Phillips 66",
           "N. Commerce City-HEP Terminal")
# the 2023 Q1-Q3 packets are posted without a revision suffix on the Suncor route
# in some quarters; the manifest below is the exact set this study reads.
packets <- unlist(lapply(names(REV), function(q)
  vapply(ROUTE, function(r) sprintf("%s_%s%s.xlsx", q, r, REV[[q]]), character(1))))

readmes <- c("HB21-1189 2023 Q1 read me v2.pdf", "HB21-1189 2023 Q2 read me v2.pdf",
             "HB21-1189 2023 Q3 read me v2.pdf", "HB21-1189 2023 Q4 read me v2.pdf",
             "HB21-1189 2024 Q1 read me v2.pdf", "HB21-1189 2024 Q2 read me v2.pdf",
             "HB21-1189 2024 Q3 read me v2.pdf", "HB21-1189 2024 Q4 read me v2.pdf",
             "HB21-1189 2025 Q1 read me v2.pdf", "HB21-1189 2025 Q2 read me v2.pdf")

enc <- function(f) {
  # the repository expects '+' for spaces and percent-encoding for the rest
  x <- utils::URLencode(f, reserved = TRUE)
  gsub("%20", "+", x, fixed = TRUE)
}

get_one <- function(fname, dest_dir) {
  dest <- file.path(dest_dir, fname)
  url  <- paste0(ROOT, enc(fname))
  tmp  <- tempfile(fileext = paste0(".", file_ext(fname)))
  okdl <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE); TRUE
  }, error = function(e) { message("  DOWNLOAD FAILED  ", fname, "  (", conditionMessage(e), ")"); FALSE })
  if (!okdl) return(data.frame(file = fname, status = "download failed",
                               bytes = NA_integer_, stringsAsFactors = FALSE))
  new_hash <- md5sum(tmp)[[1]]
  n <- file.info(tmp)$size
  if (!file.exists(dest)) {
    if (WRITE) { file.copy(tmp, dest, overwrite = TRUE); st <- "downloaded" }
    else st <- "MISSING locally (set SUNCOR_FETCH=1 to write)"
  } else if (identical(md5sum(dest)[[1]], new_hash)) {
    st <- "identical"
  } else {
    if (WRITE) { file.copy(tmp, dest, overwrite = TRUE); st <- "REPLACED (differed)" }
    else st <- "DIFFERS from the posted file"
  }
  unlink(tmp)
  data.frame(file = fname, status = st, bytes = n, stringsAsFactors = FALSE)
}

message("Repository: https://www.colorado.gov/airquality/air_toxics_repo.aspx")
message("Mode: ", if (WRITE) "download and replace" else "verify only",
        "   (SUNCOR_FETCH=", Sys.getenv("SUNCOR_FETCH", "0"), ")\n")

message("Quarterly data packets (", length(packets), "):")
res1 <- do.call(rbind, lapply(packets, function(f) {
  r <- get_one(f, DIR)
  message(sprintf("  %-58s %s", substr(f, 1, 58), r$status)); r }))

message("\nHB21-1189 read-me documents (", length(readmes), "):")
res2 <- do.call(rbind, lapply(readmes, function(f) {
  r <- get_one(f, RDIR)
  message(sprintf("  %-58s %s", substr(f, 1, 58), r$status)); r }))

res <- rbind(res1, res2)
out <- file.path(BASE, "CDPHE_download_manifest.csv")
write.csv(res, out, row.names = FALSE)
message("\nwrote ", out)

bad <- res[!res$status %in% c("identical", "downloaded", "REPLACED (differed)"), ]
if (nrow(bad)) {
  message("\n", nrow(bad), " file(s) need attention:")
  print(bad, row.names = FALSE)
} else {
  message("\nAll ", nrow(res), " files match the posted versions.")
}
