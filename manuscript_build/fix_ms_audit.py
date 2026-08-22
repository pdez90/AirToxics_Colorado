"""Corrections to the manuscript Results found by auditing every numeric claim
against the 2026-08-21 run. Each edit is marked red.

3.1  "~2 million ... over 230 unique days" -- the run has 2,602,928 measurements
     on 203 unique days (2023-02-16 to 2025-06-23), which is also what the
     Methods section already states. 230 is a transposition of 203.

3.4.2 "Maximum exceedance-days ranged from 12 to 66" -- across the 18 persistent
     groups the largest per-pollutant exceedance-day count ranges from 4
     (group 49) to 53 (group 4, toluene).

3.4.2 "Group 4 ... 179 group-days" and "Group 9 is the most persistent hotspot
     ... 159 group-days" -- neither number is produced by any metric in the run,
     and Group 4, not Group 9, is the most persistent on every metric (53 max
     pollutant-days, 56 distinct days, 145 pollutant-days summed; Group 9 gives
     27 / 41 / 93). Group 9 is the most heavily *sampled* hotspot, with 37,708
     measurements within 100 m -- that figure is correct and is what makes it
     the least directionally specific.

3.4.2 The nine petroleum-VOC groups are not all four-aromatic mixtures: groups 3
     and 16 carry no toluene, and 38, 42 and 43 no benzene.

3.6  The well-posed scenario means span 415 to 2,304 metric tons/yr over all 26;
     1,774 is the maximum once the no-reflections variant is set aside.
"""
import sys
sys.path.insert(0, "/tmp")
from redfix import red_replace, count

UP = "/tmp/msfix/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

E = [
 ("3.1 count",
  "~ 2 million mobile measurements",
  "~2.6 million mobile measurements"),
 ("3.1 days",
  "S and HCN over 230 unique days between Feb 16, 2023 and June 23, 2025",
  "S and HCN over 203 unique days between Feb 16, 2023 and June 23, 2025"),
 ("3.4.2 range",
  "Maximum exceedance-days ranged from 12 to 66, indicating substantial variability in recurrence.",
  "Maximum exceedance-days ranged from 4 to 53, indicating substantial variability in recurrence."),
 ("3.4.2 group 4",
  "Group 4 is the most persistent of them, with exceedances on 179 group-days.",
  "Group 4 is the most persistent group in the study: its most frequently "
  "exceeding pollutant, toluene, was above the campaign 99th percentile on 53 "
  "days, and the group registered exceedances on 56 distinct days across "
  "pollutants."),
 ("3.4.2 group 9",
  "Group 9 is the most persistent hotspot in the study, with exceedances on 159 "
  "group-days and 37,708 mobile measurements within 100 m, and it is also the "
  "least directionally specific.",
  "Group 9 is the most heavily sampled hotspot in the study, with 37,708 mobile "
  "measurements within 100 m and exceedances on 41 distinct days, and it is also "
  "the least directionally specific."),
 ("3.4.2 aromatics",
  "These nine groups exhibited aromatic mixtures of benzene, toluene, xylene and "
  "trimethylbenzene with no reduced species.",
  "These nine groups are persistent for aromatics only, in varying combinations "
  "of benzene, toluene, trimethylbenzene and xylene, with no reduced species."),
 ("3.6 scenarios",
  "Across the full sensitivity suite, scenario means ranged from 415 to 1,774 "
  "metric tons/yr once scenarios in which the inversion is ill-conditioned are "
  "excluded.",
  "Across the 26 scenarios that remain once the ill-conditioned cases are "
  "excluded, scenario means ranged from 415 to 2,304 metric tons/yr, and from "
  "415 to 1,774 across the 25 of those that perturb an uncertain input rather "
  "than removing the reflection term from the model."),
]

for label, old, new in E:
    n = count(xml, old)
    if n != 1:
        raise SystemExit("[%s] found %d occurrences, expected 1" % (label, n))
    xml = red_replace(xml, old, new, label=label)

open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\ndocument.xml rewritten (%d bytes)" % len(xml.encode()))
