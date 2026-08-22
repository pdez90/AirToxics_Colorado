"""Correct three exceedance-day claims that conflated two different metrics.

The run produces two counts that both get called "exceedance-days":

  A. MASTER_hotspot_group_index.csv $max_n_days, from the persistence detection
     in 30_multiple_pollutant_hotspots.R. Across the 17 groups: 15 (group 70) to
     66 (group 3, benzene). This is what 34_fancy_plots_of_hotspots.R:363 scales
     the Figure 4A circles by, and the rendered legend (20/30/40/50/60) confirms
     it.

  B. group_<id>_pollutant_highday_table.csv $n_days_high -- days on which a
     measurement within 100 m of the group centroid exceeded the CAMPAIGN 99th
     percentile. Per-group maxima run 5 to 53. This is what Table S5.1 prints,
     and its wording ("above the campaign 99th percentile") is definition B.

An earlier pass in this session replaced the submitted definition-B numbers in
3.4.2 with definition-A numbers, which contradicted Table S5.1 on the same page:
the table says Group 4 toluene 53, the text had been changed to 61. The
definition-B values are restored here and corrected against the current run.

Also corrected: "distinct days across pollutants" was previously described as
unrecoverable. It is not -- group_<id>_pollutant_highday_table.csv carries a
`days_high` column listing the dates, so the union across pollutants is exact.
Group 4: 60 (submitted 56). Group 9: 42 (submitted 41).

3.4's Figure 4A sentence keeps definition A, which is correct for that figure,
and now says so, because Table S5.1 is cited two sentences later with much
smaller numbers.

Run from /tmp/msfix with unpacked/ present.
"""
import redfix
from redfix import red_replace as R

P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()

# ---- 3.4: keep definition A for Figure 4A, but name it -------------------
x = R(x, ", indicating substantial variability in recurrence.",
         ", indicating substantial variability in recurrence. That count comes from the "
         "persistence detection that defines the groups; the per-pollutant counts in Table "
         "S5.1 apply a stricter test, days on which a measurement within 100 m exceeded the "
         "campaign 99th percentile, and are correspondingly lower.",
         "3.4 which exceedance-day metric")

# ---- 3.4.2 Group 4: restore definition B, correct the distinct-day count --
x = R(x, "Group 4 accumulates more pollutant-days of exceedance than any other group: its "
         "most frequently exceeding pollutant, toluene, was above the campaign 99th "
         "percentile on 61 days, and across its four pollutants the group totals 179 "
         "pollutant-days. Group 3 records the highest count for any single pollutant, "
         "benzene on 66 days.",
         "Group 4 is the most persistent group in the study: its most frequently exceeding "
         "pollutant, toluene, was above the campaign 99th percentile within 100 m on 53 "
         "days, and the group registered exceedances on 60 distinct days across pollutants.",
         "3.4.2 Group 4")

# ---- 3.4.2 Group 9 -------------------------------------------------------
x = R(x, "with 37,669 mobile measurements within 100 m; its most frequently exceeding "
         "pollutant, H2S, was above threshold on 44 days, and it is also the least "
         "directionally specific.",
         "with 37,669 mobile measurements within 100 m and exceedances on 42 distinct days, "
         "and it is also the least directionally specific.",
         "3.4.2 Group 9")

open(P, "w", encoding="utf-8").write(x)
print("document.xml rewritten (%d bytes)" % len(x))
