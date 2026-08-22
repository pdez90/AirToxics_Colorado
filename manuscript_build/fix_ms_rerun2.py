"""Manuscript edits for the 2026-08-22 re-run.

Every number below was recomputed in this session from the run's own outputs:
  MASTER_hotspot_group_index.csv            (17 groups, max_n_days 15-66)
  hotspot_descriptive_summary.csv           (n within 100 m, TRI distances)
  cent_out_<pollutant>_persistent.csv       (per-pollutant exceedance days;
                                             membership reconstructed by
                                             nearest-centroid and validated
                                             against the master index for 17/17
                                             groups on total_n_days, max_n_days
                                             and total_measurements)
  hotspot_source_directional_metrics.csv    (enrichment ratios)
  WWTP_H2S_plume_step_counts.csv            (37 candidates -> 4 retained)
  WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv     (per-plume rates)
  WWTP_H2S_inversion_summary_mean_ci_METRIC_TPY.csv   (mean, CI, scenarios)
  segment500_summaries_acrossSites.RData    (Figure 2 cell statistics)

Targets are kept inside a single <w:r> because a phrase spanning runs cannot be
matched. Run from /tmp/msfix with unpacked/ present.
"""
import redfix
from redfix import red_replace as R, red_replace_after as RA

P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()

# ---------------------------------------------------------------- abstract
x = R(x, "only three plume intercepts met all quality-assurance criteria",
         "only four plume intercepts met all quality-assurance criteria",
         "abstract intercepts")
x = R(x, "(1,146 metric tons/yr; 95% CI -735 to 3,027)",
         "(1,036 metric tons/yr; 95% CI -8 to 2,080)",
         "abstract mean/CI")

# ------------------------------------------------- 3.1 Figure 2 statistics
# trimethylbenzene puts 11 cells above 0.8 ppb, not "one or two"
x = R(x, "For each of these three aromatics only one or two mapped cells exceed 0.8 ppb, "
         "so the high end of the distribution rests on a handful of cells rather than on a "
         "broad band",
         "Toluene and xylene each place only one or two mapped cells above 0.8 ppb and "
         "trimethylbenzene eleven, so the high end of the distribution rests on a small "
         "minority of cells rather than on a broad band",
         "3.1 aromatic tail")
# that sentence describes the concentration map, which is Figure 2
x = RA(x, "rests on a small minority of cells", "Figure 3", "Figure 2",
          "3.1 figure cross-reference")

# reduced species: every statistic below is from the current Figure 2 input
x = R(x, "S median-of-median concentrations ranged from -1.35 to 3.00 ppb, and are "
         "displayed on a scale spanning -0.06 to 1.43 ppb",
         "S median-of-median concentrations ranged from -1.35 to 2.45 ppb across the 488 "
         "cells sampled on at least three days, and are displayed on a scale spanning 0.00 "
         "to 1.35 ppb",
         "3.1 H2S statistics")
x = R(x, ", with the 8% of mapped cells at or above 1 ppb",
         ", with the 17% of mapped cells at or above 1 ppb",
         "3.1 H2S exceedance fraction")
x = R(x, "with a median mapped value of 1.0 ppb and a display scale of 0.25 to 1.00 ppb",
         "with a median mapped value of 0.75 ppb across 359 cells and a display scale of "
         "0.00 to 1.00 ppb",
         "3.1 HCN median/scale")
x = R(x, "(maximum 4.25 ppb; 1% of cells at or above 1.2 ppb)",
         "(maximum 4.25 ppb; 3 of 359 cells, under 1%, at or above 1.2 ppb)",
         "3.1 HCN tail")

# ------------------------------------------------------ 2.5.5 plume methods
x = R(x, "the criterion could be applied to 2 of 13 candidate events",
         "the criterion could be applied to 3 of 15 candidate events",
         "methods wind-consistency")
x = R(x, "After filtering, three plume events were retained for analysis.",
         "After filtering, four plume events were retained for analysis.",
         "methods retained")

# ----------------------------------------------------------- 3.4 hotspots
x = R(x, "We identified 18 persistent hotspot groups",
         "We identified 17 persistent hotspot groups",
         "3.4 group count")
x = R(x, "Maximum exceedance-days ranged from 4 to 53",
         "Maximum exceedance-days ranged from 15 to 66",
         "3.4 exceedance-day range")

# directional enrichment
x = R(x, "Group 30 toward WWTF2 (enrichment ratio 4.3), followed by Group 11 toward "
         "Sinclair (3.7) and Group 34 toward WWTF2 (2.3)",
         "Group 30 toward WWTF2 (enrichment ratio 4.2), followed by Group 11 toward "
         "Sinclair (3.6) and Group 34 toward WWTF2 (2.3)",
         "3.4 top three alignments")
x = R(x, "as do Groups 28 (1.9), 38 (1.6) and 42 (1.5)",
         "as do Groups 28 (1.9), 38 (1.5) and 42 (1.6)",
         "3.4 WWTF1 secondary ratios")
x = R(x, "Groups 9 and 12 show no directional preference at all",
         "Groups 9 and 70 show no directional preference at all",
         "3.4 no-preference groups")

# ---------------------------------------------- 3.4.2 petroleum VOC groups
x = R(x, "Petroleum VOC hotspots (Groups 3, 4, 8, 12, 16, 23, 38, 42, 43)",
         "Petroleum VOC hotspots (Groups 3, 4, 8, 10, 16, 23, 38, 42, 43)",
         "3.4.2 petroleum list")
x = R(x, "Group 4 is the most persistent group in the study: its most frequently exceeding "
         "pollutant, toluene, was above the campaign 99th percentile on 53 days, and the "
         "group registered exceedances on 56 distinct days across pollutants.",
         "Group 4 accumulates more pollutant-days of exceedance than any other group: its "
         "most frequently exceeding pollutant, toluene, was above the campaign 99th "
         "percentile on 61 days, and across its four pollutants the group totals 179 "
         "pollutant-days. Group 3 records the highest count for any single pollutant, "
         "benzene on 66 days.",
         "3.4.2 Group 4 persistence")
x = R(x, "Groups 38, 42 and 43 lie closest to TRI facilities, between 0.25 and 0.65 km, "
         "and are ",
         "Group 10 lies closest to a TRI facility of any group in the study, at 0.19 km; "
         "Groups 42, 38 and 43 follow at 0.25, 0.51 and 0.65 km and are ",
         "3.4.2 TRI distances")

# --------------------------------------------- 3.4.2 reduced-species groups
x = R(x, "Reduced-species hotspots (Groups 9, 10, 11, 30, 34, 49, 52)",
         "Reduced-species hotspots (Groups 9, 11, 12, 30, 34, 70)",
         "3.4.2 reduced-species list")
x = R(x, "These seven groups carry H2S and/or HCN alongside aromatics.",
         "These six groups carry H2S and/or HCN alongside aromatics.",
         "3.4.2 reduced-species count")
x = R(x, "with 37,708 mobile measurements within 100 m and exceedances on 41 distinct days, "
         "and it is also the least directionally specific.",
         "with 37,669 mobile measurements within 100 m; its most frequently exceeding "
         "pollutant, H2S, was above threshold on 44 days, and it is also the least "
         "directionally specific.",
         "3.4.2 Group 9")
x = R(x, "Groups 30, 34, 49 and 52 all align most strongly with WWTF2, and Group 10 lies "
         "0.17 km from the nearest TRI facility, the closest of any group.",
         "Groups 30 and 34 align most strongly with WWTF2, while Groups 11 and 12 align "
         "with Sinclair. Group 70, which is new in this analysis, carries H2S with "
         "trimethylbenzene and xylene 1.63 km from the nearest TRI facility and shows no "
         "directional preference.",
         "3.4.2 reduced-species alignments")

x = R(x, "with four of the seven reduced-species groups aligning most strongly with WWTF2.",
         "with two of the six reduced-species groups aligning most strongly with WWTF2, by "
         "margins of 4.2 and 2.3.",
         "3.4.2 WWTF2 fraction")

# ------------------------------------------------------------ Figure 4 caption
x = R(x, "Regional map of 18 persistent hotspot groups",
         "Regional map of 17 persistent hotspot groups",
         "Figure 4 caption count")

# ------------------------------------------------------------- 3.6 emissions
x = R(x, "we identified 33 candidate H", "we identified 37 candidate H",
         "3.6 candidate count")
x = R(x, "S plume events, of which three passed all quality-assurance filters",
         "S plume events, of which four passed all quality-assurance filters",
         "3.6 retained count")
x = R(x, "The three retained intercepts gave 471, 1,003 and 1,964 metric tons/yr, a mean "
         "of 1,146 metric tons/yr with a 95% confidence interval of -735 to 3,027. With "
         "only three intercepts the interval spans zero,",
         "The four retained intercepts gave 471, 706, 1,003 and 1,964 metric tons/yr, a "
         "mean of 1,036 metric tons/yr with a 95% confidence interval of -8 to 2,080. With "
         "only four intercepts the interval still includes zero,",
         "3.6 rates and mean")
x = R(x, "scenario means ranged from 415 to 2,304 metric tons/yr, and from 415 to 1,774 "
         "across the 25",
         "scenario means ranged from 420 to 2,223 metric tons/yr, and from 420 to 1,687 "
         "across the 25",
         "3.6 scenario ranges")
x = R(x, "Given three intercepts and a confidence interval that spans zero,",
         "Given four intercepts and a confidence interval that still includes zero,",
         "3.6 limitations framing")

# ------------------------------------------------------------- 4 Discussion
x = R(x, "These discrepancies are not simply technical, they have direct regulatory "
         "implications.",
         "These discrepancies are not simply technical; they have direct regulatory "
         "implications.",
         "discussion comma splice")
# the inversion flags a discrepancy; it does not identify emission magnitudes
x = R(x, "S analysis demonstrates how mobile monitoring combined with inverse modeling can "
         "identify emission magnitudes that may not be fully captured in reported "
         "inventories,",
         "S analysis shows, more tentatively, how mobile monitoring combined with inverse "
         "modeling can flag a discrepancy between inferred and reported emission magnitudes "
         "that warrants follow-up,",
         "discussion H2S claim")
# put the numbers into the paragraph that makes the accountability argument
x = R(x, " metric tons/yr, and, despite uncertainty, were large relative to reported "
         "emissions for individual facilities in the corridor. This comparison rests on "
         "three plume intercepts whose mean carries a confidence interval spanning zero,",
         " metric tons/yr, averaging 1,036 metric tons/yr across four intercepts that span "
         "471 to 1,964 metric tons/yr, and, despite uncertainty, were large relative to "
         "reported emissions for individual facilities in the corridor. This comparison "
         "rests on four intercepts whose mean carries a confidence interval that still "
         "includes zero,",
         "discussion emission numbers")

open(P, "w", encoding="utf-8").write(x)
print("document.xml rewritten (%d bytes)" % len(x))

# ------------------------------------------------- appended: Group 14 tie
# hotspot_source_directional_metrics.csv: Group 14's top alignment is a
# three-way tie at 1.48 between the woodshop, a refuelling location and
# Phillips 66 -- the covered facility was omitted.
x = open(P, encoding="utf-8").read()
x = R(x, "Group 14 aligns with the woodshop and a refuelling location in equal measure, "
         "while Group 28 aligns with WWTF1.",
         "Group 14 aligns equally with the woodshop, a refuelling location and Phillips 66 "
         "(1.5 in each case), while Group 28 aligns with WWTF1 (1.9).",
         "3.4.2 Group 14 tie")
open(P, "w", encoding="utf-8").write(x)
print("Group 14 edit applied")
