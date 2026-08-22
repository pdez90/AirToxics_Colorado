"""SI S1.4: the HCN calibration-window drop count.

The run's own diagnostic (03_checks_flags.R, 01_run_all.log) reports

    [HCN] 2025-05-28..30 calibration window removed: 27,967 of 514,118 values

The SI said 29,085, which is not a number this run produces.
"""
import redfix
from redfix import red_replace as R
P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()
x = R(x, "two of them carry measurements, and 29,085 HCN values are dropped.",
         "two of them carry measurements, and 27,967 of 514,118 HCN values are dropped.",
         "S1.4 HCN calibration-window count")
open(P, "w", encoding="utf-8").write(x)
print("done")
