"""Name the exceedance-day metric wherever a number is quoted.

Re-derived from the latest files (MASTER_hotspot_group_index.csv and the 17
group_<id>_pollutant_highday_table.csv):

  A  MASTER max_n_days, what Figure 4A scales circles by : 15 (g70) to 66 (g3)
  B  max n_days_high within 100 m vs campaign p99,
     what Table S5.1 prints                              :  5 (g23) to 53 (g4, toluene)
  U  distinct days across pollutants (union of days_high):  8 (g23) to 60 (g4)

All three are current. The remaining risk was presentational: Figure 4A's
caption did not say which metric it used, so a reader comparing the 15-66 in
3.4 against Table S5.1's smaller per-pollutant counts had no way to reconcile
them. Both places now name the metric and give the other one's range.
"""
import redfix
from redfix import red_replace as R
P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()

x = R(x, "and are correspondingly lower.",
         "and range from 5 to 53 days.",
         "3.4 give the Table S5.1 range")

x = R(x, "shown as circles scaled by the maximum number of exceedance-days observed for any "
         "pollutant within each group.",
         "shown as circles scaled by the maximum number of exceedance-days observed for any "
         "pollutant within each group, taken from the persistence detection that defines the "
         "groups and ranging from 15 to 66 days. The per-pollutant counts in Table S5.1 "
         "apply a stricter within-100 m test against the campaign 99th percentile and range "
         "from 5 to 53.",
         "Figure 4A caption metric")

open(P, "w", encoding="utf-8").write(x)
print("done")
