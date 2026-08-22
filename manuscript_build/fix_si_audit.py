"""Corrections to SI sections S2, S3 and S6 found by auditing every numeric
claim against the 2026-08-21 run. Each edit is marked red.

S2  Both instrument paragraphs pointed readers to Table S3.1 (descriptive
    statistics of the mobile measurements). The instrument specifications are
    in Table S1.1.

S3  The hour-of-day bins were labelled an hour early. The listed fractions
    (0.2, 2.9, 12.7, ...) belong to 7-8 am onward, not 6-7 am onward: the run
    has no measurements before 7 am, and the 9 am - 2 pm bins as labelled sum
    to 82%, not the 88% the same paragraph states. Relabelled and refreshed
    from the run (which gives 88.2% for 9 am - 2 pm).

S3  H2S/HCN-to-aromatic correlations are below 0.15 on the Holly route
    (max 0.103) but not on the Suncor route, where HCN-benzene and
    HCN-trimethylbenzene both reach 0.171.

S6  Two sentences still described "the 7 plumes"; the run retains 3.

S6  The retained intercepts are at 1.95, 3.55 and 4.30 km, not 1.2 to 4.5 km,
    and the January event is the smallest of the three rather than intermediate.

S6  The 26 well-posed scenario means span 415 to 2,304 metric tons/yr. 1,774 is
    the largest once the no-reflections variant is set aside, which leaves 25.
"""
import sys
sys.path.insert(0, "/tmp")
from redfix import red_replace, red_replace_after, count

UP = "/tmp/sifig/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

print("== S2: instrument cross-references ==")
xml = red_replace_after(xml, "please refer to ", "Table S3.1", "Table S1.1",
                        label="S2 Vocus Elf")
xml = red_replace_after(xml, "this instrument is shown in ", "Table S3.1", "Table S1.1",
                        label="S2 Vocus 2R")

print("\n== S3: hour-of-day bins ==")
OLD = ("The fraction of overall measurements made between 6 and 7 am, 7- 8 am, "
       "8 - 9 am, 9 - 10 am, 10 - 11 am, 11 am - noon, noon - 1 pm, 1 to 2 pm, "
       "2 to 3 pm, and 3 to 4 pm are 0.2%, 2.9%, 12.7%, 19.0%, 20.9%, 20.5%, "
       "15.3%, 6.3%, 1.6%, 0.5% respectively.")
NEW = ("No measurements were made before 7 am. The fraction of overall "
       "measurements made between 7 - 8 am, 8 - 9 am, 9 - 10 am, 10 - 11 am, "
       "11 am - noon, noon - 1 pm, 1 - 2 pm, 2 - 3 pm, 3 - 4 pm and 4 - 5 pm "
       "are 0.2%, 3.0%, 12.7%, 19.0%, 20.8%, 20.4%, 15.2%, 6.3%, 1.7% and 0.6% "
       "respectively.")
assert count(xml, OLD) == 1
xml = red_replace(xml, OLD, NEW, label="S3 hour bins")

print("\n== S3: H2S/HCN correlation bound ==")
xml = red_replace(xml, "(r< 0.15), along both routes",
                  "(r < 0.2 on the Suncor and Phillips 66 route, r < 0.11 on the "
                  "Holly Energy Partners (Sinclair) Terminal route)",
                  label="S3 r bound")

print("\n== S6: plume count ==")
xml = red_replace(xml, "the Gaussian plume inversion for the 7 plumes was re-run",
                  "the Gaussian plume inversion for the 3 retained plumes was re-run",
                  label="S6 7->3 (a)")
xml = red_replace(xml, "emissions rate calculations for our 7 plumes",
                  "emissions rate calculations for our 3 retained plumes",
                  label="S6 7->3 (b)")

print("\n== S6: distances and per-event rates ==")
xml = red_replace(xml, "from roughly 1.2 to 4.5 km", "at 1.95, 3.55 and 4.30 km",
                  label="S6 distances")
xml = red_replace(xml,
    "Plumes sampled in January and May generally yielded intermediate estimates, "
    "while the November event produced a notably larger inferred rate than the "
    "rest of the sample.",
    "The January 2024 and May 2024 events gave 471 and 1,003 metric tons/yr "
    "respectively, while the November 2023 event produced a notably larger "
    "inferred rate of 1,964 metric tons/yr.",
    label="S6 event months")

print("\n== S6: well-posed scenario range ==")
xml = red_replace(xml,
    "Across the 26 scenarios that remain well-posed once ill-conditioned cases "
    "are excluded, mean estimates range from 415 to 1,774 metric tons/yr",
    "Across the 26 scenarios that remain well-posed once ill-conditioned cases "
    "are excluded, mean estimates range from 415 to 2,304 metric tons/yr, and "
    "from 415 to 1,774 across the 25 of them that perturb an uncertain input "
    "rather than removing the reflection term from the model",
    label="S6 scenario range")

open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\ndocument.xml rewritten (%d bytes)" % len(xml.encode()))
