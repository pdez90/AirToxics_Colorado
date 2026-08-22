"""Manuscript: report the four intercepts and their range instead of a
confidence interval on the mean.

Why: the four baseline intercepts (471, 706, 1,003, 1,964 metric tons/yr) are
all positive, as is every one of the 112 per-plume estimates across all 28
scenarios (minimum 126 t/yr). The -8 lower bound was an artifact of a symmetric
t-interval on n = 4: t(0.975, 3) = 3.182 and se = 328.1, so the margin (1,044.2)
overshoots the mean (1,036.0) by 8 t/yr. A normal-theory interval is the wrong
shape for a strictly positive, right-skewed quantity -- 21 of the 28 scenarios
carry a negative lower bound for the same reason -- and n = 4 does not support
one. The range is reported instead.

Run from /tmp/msfix with unpacked/ present.
"""
import redfix
from redfix import red_replace as R

P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()

# ---------------------------------------------------------------- abstract
x = R(x, " and the resulting mean is not statistically distinguishable from zero ",
         " and the individual estimates span a factor of four ",
         "abstract framing")
x = R(x, "(1,036 metric tons/yr; 95% CI -8 to 2,080)",
         "(471 to 1,964 metric tons/yr; mean 1,036), too few for a formal uncertainty "
         "interval",
         "abstract range")

# ------------------------------------------------------------- 3.6 baseline
x = R(x, "The four retained intercepts gave 471, 706, 1,003 and 1,964 metric tons/yr, a "
         "mean of 1,036 metric tons/yr with a 95% confidence interval of -8 to 2,080. With "
         "only four intercepts the interval still includes zero,",
         "The four retained intercepts gave 471, 706, 1,003 and 1,964 metric tons/yr, a "
         "range spanning a factor of four around a mean of 1,036 metric tons/yr. We report "
         "that range rather than a confidence interval: four intercepts do not support a "
         "formal uncertainty statement, and a symmetric interval on four positive, "
         "right-skewed values extends below zero even though every individual estimate is "
         "positive,",
         "3.6 baseline result")

# ------------------------------------------------------- 3.6 attribution framing
x = R(x, "Given four intercepts and a confidence interval that still includes zero,",
         "Given four intercepts spanning 471 to 1,964 metric tons/yr,",
         "3.6 limitations framing")

# ------------------------------------------------------------- 4 Discussion
x = R(x, "This comparison rests on four intercepts whose mean carries a confidence interval "
         "that still includes zero,",
         "This comparison rests on four intercepts spanning a factor of four,",
         "discussion framing")

open(P, "w", encoding="utf-8").write(x)
print("document.xml rewritten (%d bytes)" % len(x))
