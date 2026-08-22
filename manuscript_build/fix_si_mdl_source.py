"""Point the SI at the CDPHE source of record for the audit MDLs, and give the
HCN exclusion the reason CDPHE publishes for it.

The HB21-1189 Q1 2025 read-me states: "HCN data prior to January 22, 2025
received empirical background correction. Data from January 22, 2025 to present
represent absolute measurements." That is the justification for the cutoff in
03_checks_flags.R, which until now carried no comment.
"""
import sys
sys.path.insert(0, "/tmp")
from redfix import red_replace, count

UP = "/tmp/sifig/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

E = [
 ("S1.2 caption",
  ": Method Detection Limits and LODs for the CAT and EMU labs",
  ": Audit method detection limits for the CAT and EMU mobile "
  "laboratories, by quarter. Values are read directly from the Quarterly Summary "
  "tab of each CDPHE quarterly data packet by R_scripts/69_cdphe_audit_mdls.R, "
  "which writes CDPHE_audit_MDLs.csv; the HB21-1189 read-me documents name that "
  "tab as the source of record for method detection limits. The EMU laboratory "
  "joined the campaign in the fourth quarter of 2023, so it has no entries "
  "before then. Two-second limits of detection are reported as n/a throughout "
  "the study period."),

 ("S3.1 caption MDL basis",
  "The below-MDL fraction uses the audit MDLs in Table S1.2, applied by "
  "lab and by period, with the last listed period carried forward past March 2025.",
  "The below-MDL fraction uses the quarterly audit MDLs in Table S1.2, applied "
  "by laboratory and by quarter; they are read from the CDPHE packets rather "
  "than transcribed, and they cover every quarter of the study period, so no "
  "measurement is left without a published limit."),

 ("S3.1 caption HCN reason",
  "with everything on or before 22 January 2025 - which is why HCN is represented "
  "on 41 sampling days against about 165 for the other five.",
  "with everything on or before 22 January 2025. CDPHE's HB21-1189 read-me for "
  "the first quarter of 2025 records that HCN measurements before 22 January 2025 "
  "carry an empirical background correction, while those from that date onward "
  "are absolute; the cutoff keeps only the absolute measurements, which is why "
  "HCN is represented on 41 sampling days against about 165 for the other five."),
]

for label, old, new in E:
    n = count(xml, old)
    if n != 1:
        raise SystemExit("[%s] found %d occurrences, expected 1" % (label, n))
    xml = red_replace(xml, old, new, label=label)

open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\ndocument.xml rewritten (%d bytes)" % len(xml.encode()))
