"""Correct the below-MDL claims again, this time against the CDPHE audit MDLs
read out of the quarterly packets.

My previous pass used the MDL table as transcribed into SI Table S1.2, which
listed EMU hydrogen cyanide as 0.18 ppbV. The packets give 18.0. With the source
values the fractions are benzene 93.1%, H2S 97.5%, HCN 96.4% - so the submitted
sentence "more than 90% of benzene, H2S and HCN" was right, and the 53% figure I
put in was an artefact of the typo. Stating the three numbers explicitly now, so
the claim is checkable rather than approximate.

The HCN coverage caveat stands, and gains CDPHE's own reason for the cutoff:
measurements before 22 January 2025 carry an empirical background correction;
from that date they are absolute.
"""
import sys
sys.path.insert(0, "/tmp")
from redfix import red_replace, count

UP = "/tmp/msfix/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

E = [
 ("3.1 below-MDL",
  ". We note that 95% of benzene and 98% of H",
  ". We note that 93% of benzene, 98% of H"),
 ("3.1 below-MDL b",
  "S measurements were below the audit method detection limit, as were 53% of "
  "HCN measurements (",
  "S and 96% of HCN measurements were below the quarterly audit method detection "
  "limits published by CDPHE ("),
 ("limitations MDL",
  "Most mobile measurements of benzene (95%) and H",
  "Most mobile measurements fell below the audit method detection limits - 93% "
  "of benzene, 98% of H"),
 ("limitations MDL b",
  "S (98%) fell below the audit method detection limits, as did 53% of HCN "
  "measurements, indicating that much of the observed variability is dominated "
  "by noise at low concentrations.",
  "S and 96% of HCN - indicating that much of the observed variability is "
  "dominated by noise at low concentrations."),
 ("limitations HCN reason",
  "HCN carries a further limitation of coverage: the pipeline retains HCN only "
  "after 22 January 2025, so it rests on 41 sampling days in a single spring "
  "season rather than the full campaign, and its spatial and seasonal patterns "
  "should be read accordingly.",
  "HCN carries a further limitation of coverage. CDPHE reports that HCN "
  "measurements before 22 January 2025 carry an empirical background correction "
  "and that only those from that date onward are absolute, so we retain HCN only "
  "after that date. It therefore rests on 41 sampling days in a single spring "
  "season rather than on the full campaign, and its spatial and seasonal patterns "
  "should be read accordingly."),
]

for label, old, new in E:
    n = count(xml, old)
    if n != 1:
        raise SystemExit("[%s] found %d occurrences, expected 1" % (label, n))
    xml = red_replace(xml, old, new, label=label)

open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\ndocument.xml rewritten (%d bytes)" % len(xml.encode()))
