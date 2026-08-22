"""Two paragraphs that the re-run never reached.

2.5.3.2 (p089) still carried the as-submitted DBSCAN pipeline description.
Verified against hotspot_thresholds_summary.csv from the 2026-08-21/22 run and
28_hotspot_analysis_identifying_most_persistent_hotspots.R:120-121:

  pollutant          clusters  n cutoff  day cutoff  persistent
  benzene                 416      63.5          12          35
  toluene                 423      44.8           8          33
  trimethylbenzene        483      39.4           6          32
  xylene                  465      49.0           8          35
  H2S                     602      39.0          13          58
  HCN                     324      24.4           4          23
  total                 2,713                                216

"160 initial clusters" was the submitted figure; the current run gives 2,713.
The persistence rule was also described incorrectly: the script takes the 90th
percentile of each pollutant's own cluster distribution (`quantile(..., 0.90)`
on `n` and on `n_days`), i.e. the top 10% of CLUSTERS on both axes -- not "10%
of total high-concentration measurements".

3.1 (p116) still reported the pre-re-run analysis-set span. Table S3.1 gives
1,461,849-1,553,754 for the aromatics and H2S; the aromatics run 159-165
sampling days but H2S now spans 199, because it sits on the Picarro and is
unaffected by the 2023 inlet contamination that the Eiger aromatics lose. HCN
is 481,471 values on 39 days after the 28-30 May 2025 calibration window was
dropped, not 509,210 on 41.

Run from /tmp/msfix with unpacked/ present.
"""
import redfix
from redfix import red_replace as R

P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()

# ---- 2.5.3.2 clustering pipeline ----------------------------------------
x = R(x, "Application of DBSCAN yielded 160 initial clusters. To distinguish persistent "
         "clusters from transient signals, clusters were classified as persistent if they "
         "exceeded both (1) 10% of total high-concentration measurements for that pollutant "
         "and (2) 10% of sampling days with any high-concentration events.",
         "Application of DBSCAN yielded 2,713 initial clusters across the six pollutants "
         "(416 benzene, 423 toluene, 483 trimethylbenzene, 465 xylene, 602 H2S and 324 "
         "HCN). To distinguish persistent clusters from transient signals, clusters were "
         "classified as persistent if they fell in the top 10% of that pollutant's own "
         "cluster distribution on both (1) the number of high-concentration measurements "
         "and (2) the number of days carrying them, that is, above the 90th percentile of "
         "each.",
         "2.5.3.2 cluster count and rule")
x = R(x, "These criteria corresponded to pollutant-specific thresholds (e.g., n = 63.8 and "
         "12 days for benzene; n = 43 and 7 days for toluene; n = 36 and 5 days for "
         "trimethylbenzene; n = 53.5 and 8 days for xylene; n = 28 and 9 days for H",
         "These criteria corresponded to pollutant-specific thresholds (n = 63.5 and 12 "
         "days for benzene; n = 44.8 and 8 days for toluene; n = 39.4 and 6 days for "
         "trimethylbenzene; n = 49 and 8 days for xylene; n = 39 and 13 days for H",
         "2.5.3.2 thresholds")
x = R(x, "S; n = 24 and 3 days for HCN).",
         "S; n = 24.4 and 4 days for HCN), retaining 35, 33, 32, 35, 58 and 23 clusters "
         "respectively.",
         "2.5.3.2 HCN threshold and retained counts")

# ---- 3.1 analysis-set span ----------------------------------------------
x = R(x, " the analysis set holds 1.38-1.55 million values of each aromatic and of H2S, "
         "spanning 159-165 sampling days apiece; HCN is the exception, with 509,210 values "
         "confined to 41 days between 22 January and 23 June 2025.",
         " the analysis set holds 1.46-1.55 million values of each aromatic and of H2S. The "
         "aromatics span 159-165 sampling days each; H2S spans 199, because it is measured "
         "on the Picarro and so is unaffected by the 2023 inlet contamination that removes "
         "those days from the Eiger aromatics. HCN is the exception, with 481,471 values "
         "confined to 39 days between 22 January and 23 June 2025.",
         "3.1 analysis-set span")

open(P, "w", encoding="utf-8").write(x)
print("document.xml rewritten (%d bytes)" % len(x))

# ---- appended: 2.5.3.1 H2S 99th-percentile threshold ---------------------
# hotspot_thresholds_summary.csv b99 and Table S3.1's 99th-percentile row both
# give 4.8 ppb for H2S in this run. 4.6 was the 2026-08-19 vintage, before the
# 2023 inlet window was retained.
x = open(P, encoding="utf-8").read()
x = R(x, "4.6 ppb for H", "4.8 ppb for H",
         "2.5.3.1 H2S p99 threshold")
open(P, "w", encoding="utf-8").write(x)
print("H2S threshold corrected")
