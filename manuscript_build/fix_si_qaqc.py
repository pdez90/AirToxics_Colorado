"""Rewrite SI section S1.4 so it describes what the pipeline actually does.

Corrections, all verified against the CDPHE read-me documents and the delivered
1-second files:

* The MDL/negative substitutions the section describes are what CDPHE's read-me
  says it does, but they are NOT present in the mobile 1-second files. Of the
  MD-flagged values that carry a number, only 1-9% sit at half the audit MDL;
  the rest form a continuous distribution including 191,962 benzene values at
  -0.1 ppb. So there is nothing to undo, and the section's "we did not make
  these replacements" understates the point.
* The list of voided flags was missing five codes.
* BR is a null qualifier but never accompanies a value, so voiding it removes
  nothing - which is why negative values survive.
* The inlet-contamination window is applied from 16 April, not CDPHE's
  "approximately April 26", and H2S is retained across it.
* The August 2024 Eiger paragraph stated the fault but not the consequence.
* Nothing described the delay correction or the native-cadence averaging.
"""
import sys
sys.path.insert(0, "/tmp")
from redfix import red_replace, count

UP = "/tmp/sifig/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

E = [
 ("MDL substitution",
  "In this paper, however, we did not make these replacements.",
  "We make no such replacements. We also checked whether CDPHE had already made "
  "them in the files we use, and it has not: of the measurements flagged MD in "
  "the delivered one-second files, only 1-9% (depending on pollutant) sit at "
  "half the audit MDL, and the remainder form a continuous distribution that "
  "includes negative values - 191,962 benzene readings at -0.1 ppb alone. The "
  "substitution rule quoted above therefore describes CDPHE's summary products "
  "rather than the one-second record, and the sub-MDL and negative values we "
  "retain are measurements, not imputed constants."),

 ("null flag list",
  "QA Audit (BL), and Accuracy check (BM). All measurements with these flags "
  "were removed from the analysis.",
  "QA Audit (BL), accuracy check (BM), site computer or data logger down (BK), "
  "sample value below acceptable range (BR), exceeds critical criteria (EC), "
  "method blank (MB) and experimental data (XX). A measurement is voided if any "
  "token in its flag string is one of these eighteen null data qualifiers, and "
  "the same rule is applied to all six pollutants. Qualifiers that CDPHE "
  "classifies as informational (CD, CG, IH, IL, IR, IT, QG, QP, QT, QW) or as "
  "quality assurance (EH, LJ, MD, NS, QX) are retained, MD - value below the MDL "
  "- among them. In practice the rule removes very little: BR, although a null "
  "qualifier, never accompanies a reported value, because CDPHE blanks the "
  "measurement itself when it sets BR, so the only codes that actually discard a "
  "number are AL and BH, between 480 and 8,459 rows per pollutant. Negative "
  "values that CDPHE did not blank survive, 358,048 of them for benzene and "
  "627,134 for H2S."),

 ("Eiger Aug 2024",
  "Vocus Eiger instrument’s background correction was not active during the "
  "August 13-14, 2024, deployments.",
  "The Vocus Eiger instrument’s background correction was not active during the "
  "13-14 August 2024 deployments, which CDPHE reports left concentrations "
  "artificially elevated because the instrument baseline was not accounted for. "
  "We removed benzene, toluene, trimethylbenzene and xylene on those two days. "
  "H2S and HCN are measured by the Picarro CRDS and the Vocus Aim CI-ToF-MS "
  "respectively, are unaffected by the Eiger baseline, and were retained."),

 ("inlet window",
  "Between April 26, 2023, and September 20, 2023, the ATOPs team noticed inlet "
  "line contamination.",
  "CDPHE discovered inlet line contamination on 20 September 2023, confirmed by "
  "visible residue in the lines, and infers from elevated daily averages that it "
  "began about 26 April 2023; the lines were replaced on 20 September 2023 and "
  "monthly averages then fell. Because the onset is inferred rather than "
  "observed, we begin the exclusion ten days earlier, on 16 April 2023, and end "
  "it on the replacement date."),

 ("inlet removal scope",
  "These data were removed from the analysis.",
  "CDPHE attributes the elevation to the compounds measured by the PTR-ToF-MS "
  "and CI-ToF-MS instruments - benzene, toluene, xylene, trimethylbenzene and "
  "HCN - and states that it is not corrected for in the delivered data. We "
  "removed those five pollutants over the window. H2S is measured by a separate "
  "Picarro analyser, is not among the compounds CDPHE identifies, and shows no "
  "elevation over the window in our own record (median 1.00 ppb both before and "
  "during, against increases of 3.0-fold for benzene, 2.0-fold for toluene, "
  "1.8-fold for xylene and 2.0-fold for HCN), so H2S measurements were retained."),

 ("additional flags lead-in",
  "The ATOPs team provided additional flags for all data.",
  "CDPHE documents three further data-quality problems in the quarterly "
  "read-me files that accompany the data packets. These advisories appear only "
  "in those read-me documents; the data workbooks themselves carry the detection "
  "limits, the summary statistics and the flag legend, but no quality notes, so "
  "the read-me files are archived alongside the packets in our repository."),
]

for label, old, new in E:
    n = count(xml, old)
    if n != 1:
        raise SystemExit("[%s] found %d occurrences, expected 1" % (label, n))
    xml = red_replace(xml, old, new, label=label)

open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\ndocument.xml rewritten (%d bytes)" % len(xml.encode()))
