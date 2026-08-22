"""Tie the measurement-count and below-MDL claims to the numbers Table S3.1 now
generates (R_scripts/70_table_s31.R -> TABLE_S3.1.csv).

The counts: 2,602,928 one-second records on 203 route-days; per pollutant the
analysis set holds 1.38-1.55 million values spanning 159-165 sampling days,
except HCN, which the pipeline restricts to after 22 January 2025 and which
therefore covers 41 days.

Below the audit MDL: benzene 95.2%, toluene 39.0%, xylene 55.5%,
trimethylbenzene 76.1%, H2S 97.5%, HCN 52.9%. "More than 90% of benzene, H2S and
HCN" and "a large fraction (~95%)" are both wrong for HCN.
"""
import sys
sys.path.insert(0, "/tmp")
from redfix import red_replace, count

UP = "/tmp/msfix/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

E = [
 ("3.1 record",
  "~2.6 million mobile measurements",
  "2,602,928 one-second records"),
 ("3.1 coverage",
  "S and HCN over 203 unique days between Feb 16, 2023 and June 23, 2025",
  "S and HCN, collected on 203 route-days between Feb 16, 2023 and June 23, "
  "2025, are summarised in Table S3.1. After QA/QC and the campaign date "
  "exclusions the analysis set holds 1.38-1.55 million values of each aromatic "
  "and of H2S, spanning 159-165 sampling days apiece; HCN is the exception, "
  "with 509,210 values confined to 41 days between 22 January and 23 June 2025. "
  "These statistics"),
 ("3.1 below-MDL",
  ". We note that more than 90% of benzene, H",
  ". We note that 95% of benzene and 98% of H"),
 ("3.1 below-MDL b",
  "S and HCN measurements were below the MDL (",
  "S measurements were below the audit method detection limit, as were 53% of "
  "HCN measurements ("),
 ("limitations MDL",
  "A large fraction (~95%) of mobile measurements for benzene, H",
  "Most mobile measurements of benzene (95%) and H"),
 ("limitations MDL b",
  "S, and HCN were below method detection limits, indicating that much of the "
  "observed variability is dominated by noise at low concentrations.",
  "S (98%) fell below the audit method detection limits, as did 53% of HCN "
  "measurements, indicating that much of the observed variability is dominated "
  "by noise at low concentrations. HCN is further limited by coverage: the "
  "pipeline retains HCN only after 22 January 2025, so it rests on 41 sampling "
  "days in a single spring season rather than the full campaign, and its "
  "spatial and seasonal patterns should be read accordingly."),
]

for label, old, new in E:
    n = count(xml, old)
    if n != 1:
        raise SystemExit("[%s] found %d occurrences, expected 1" % (label, n))
    xml = red_replace(xml, old, new, label=label)

open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\ndocument.xml rewritten (%d bytes)" % len(xml.encode()))
