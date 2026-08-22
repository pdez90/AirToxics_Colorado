"""Give the two remaining exclusion windows their published reasons in the SI.

Both come from CDPHE's own HB21-1189 read-me documents, which I had not read
when the caption was first written:

  spring/summer 2023 - inlet line contamination, discovered 20 September 2023,
    "suspected to have started ... on approximately April 26, 2023", affecting
    the PTR-ToF-MS and CI-ToF-MS compounds, "not corrected for in the data".
  13-14 August 2024 - the Vocus Eiger's automatic baseline correction was not
    operational, so concentrations were "artificially elevated".
"""
import sys
sys.path.insert(0, "/tmp")
from redfix import red_replace, count

UP = "/tmp/sifig/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

OLD = ("The campaign date exclusions are the hard-coded windows in "
       "03_checks_flags.R: 16 April - 20 September 2023 for all six pollutants, "
       "13-14 August 2024 for the four aromatics, and, for HCN, 2-3 January 2025 "
       "together with everything on or before 22 January 2025.")
NEW = ("The campaign date exclusions follow CDPHE's own advisories in the "
       "HB21-1189 read-me documents. 16 April - 20 September 2023: CDPHE reports "
       "that inlet line contamination, discovered on 20 September 2023 and "
       "confirmed by visible residue, is suspected to have begun about 26 April "
       "2023 and elevated the compounds measured by PTR-ToF-MS and CI-ToF-MS, "
       "and that it is not corrected for in the delivered data; the lines were "
       "replaced on 20 September 2023, after which monthly averages fell. We "
       "start the window ten days before CDPHE's estimated onset because that "
       "onset is inferred from elevated daily averages rather than observed "
       "directly, and we also drop H2S over the window even though it is "
       "measured by a separate Picarro analyser and is not on CDPHE's affected "
       "list. 13-14 August 2024: the Vocus Eiger's automatic baseline correction "
       "was not operational on those two deployments, so the four aromatics it "
       "measures were reported artificially elevated. For HCN, 2-3 January 2025 "
       "together with everything on or before 22 January 2025.")
assert count(xml, OLD) == 1
xml = red_replace(xml, OLD, NEW, label="S3.1 caption exclusion reasons")

open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\ndocument.xml rewritten (%d bytes)" % len(xml.encode()))
