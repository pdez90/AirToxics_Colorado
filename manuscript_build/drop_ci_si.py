"""SI Section S6: drop confidence intervals from the plume inversion, report the
range of the four intercepts and the scenario means alone.

Why: all four baseline intercepts are positive (471, 706, 1,003, 1,964 t/yr), as
is every one of the 112 per-plume estimates across all 28 scenarios (minimum
126 t/yr). The negative lower bounds were an artifact of a symmetric t-interval
on n = 4 -- t(0.975, 3) = 3.182, se = 328.1, margin 1,044.2 against a mean of
1,036.0 -- which is the wrong shape for a strictly positive, right-skewed
quantity; 21 of the 28 scenarios carried a negative lower bound for that reason.
The baseline interval is retained in the text as a labelled artifact so a reader
can see it was computed and why it is not reported.

Note: the Figure S4.3 caption also says "95% confidence intervals". That is the
TRI buffer concentration analysis, a different estimate on a large sample, and
is deliberately left alone.

Run from /tmp/sifig with unpacked/ present.
"""
import redfix
from redfix import red_replace as R

P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()

# ---------------------------------------------------------- S6.5.2 baseline
x = R(x, "inferred emission rates were 471, 706, 1,003 and 1,964 metric tons/year, with a "
         "mean of 1,036 metric tons/year (95% CI: -8, 2,080)",
         "inferred emission rates were 471, 706, 1,003 and 1,964 metric tons/year, a range "
         "spanning a factor of four around a mean of 1,036 metric tons/year",
         "S6 baseline result")
x = R(x, "With four intercepts the confidence interval still includes zero:",
         "We report that range rather than a confidence interval:",
         "S6 baseline framing")
x = R(x, "Every confidence interval quoted in this section ",
         "No confidence intervals are quoted in this section: a normal-theory interval on "
         "four positive, right-skewed observations ",
         "S6 interval policy (1/3)")
x = R(x, "is a t-interval on four observations",
         "is both very wide and not bounded below by zero",
         "S6 interval policy (2/3)")
x = R(x, " and is correspondingly wide, so the differences between scenarios below are "
         "informative",
         " - for the baseline it runs from -8 to 2,080 metric tons/yr even though every "
         "individual estimate is positive. Scenario means are therefore reported alone, and "
         "the differences between scenarios below are informative",
         "S6 interval policy (3/3)")

# ------------------------------------------------------------- source height
x = R(x, "Mean inferred emissions were 1,036 metric tons/yr (95% CI: -8, 2,080) for a 1 m "
         "source height, 1,036 metric tons/yr (-8, 2,080) for 10 m, 1,036 metric tons/yr "
         "(-8, 2,080) for 15 m, 1,036 metric tons/yr (-8, 2,081) for 20 m, 1,037 metric "
         "tons/yr (-8, 2,081) for 30 m, and 1,038 metric tons/yr (-6, 2,083) for 50 m.",
         "Mean inferred emissions were 1,036 metric tons/yr for a 1 m source height, 1,036 "
         "metric tons/yr for 10 m, 1,036 metric tons/yr for 15 m, 1,036 metric tons/yr for "
         "20 m, 1,037 metric tons/yr for 30 m, and 1,038 metric tons/yr for 50 m.",
         "S6 source-height scenarios")

# ------------------------------------------------------------------ wind speed
x = R(x, "decreased the mean estimate to 829 metric tons/yr (95% CI: -6, 1,664), whereas "
         "increasing wind speed by 20% increased the estimate to 1,243 metric tons/yr "
         "(-10, 2,496).",
         "decreased the mean estimate to 829 metric tons/yr, whereas increasing wind speed "
         "by 20% increased the estimate to 1,243 metric tons/yr.",
         "S6 wind-speed scenarios")

# ------------------------------------------------------------------- stability
x = R(x, "increased the mean emission estimate to 1,314 metric tons/yr (95% CI: 438, 2,191), "
         "whereas",
         "increased the mean emission estimate to 1,314 metric tons/yr, whereas",
         "S6 stability CAT-1")
x = R(x, "reduced the estimate to 420 metric tons/yr (64, 777).",
         "reduced the estimate to 420 metric tons/yr.",
         "S6 stability CAT+1")

# ------------------------------------------------------------ downwind distance
x = R(x, "decreased the mean estimate to 891 metric tons/yr (95% CI: 98, 1,683), while "
         "increasing the distance by 10% increased the estimate to 1,191 metric tons/yr "
         "(-144, 2,526).",
         "decreased the mean estimate to 891 metric tons/yr, while increasing the distance "
         "by 10% increased the estimate to 1,191 metric tons/yr.",
         "S6 distance scenarios")

# ---------------------------------------------------------- crosswind geometry
x = R(x, "gives 905 metric tons/yr (95% CI: -205, 2,015); shifting the bearing by 5 degrees "
         "gives 917 (-201, 2,035) or 1,687 (829, 2,544) depending on direction, and by 10 "
         "degrees 1,111 (-34, 2,256) or 4,717 (2,329, 7,104).",
         "gives 905 metric tons/yr; shifting the bearing by 5 degrees gives 917 or 1,687 "
         "depending on direction, and by 10 degrees 1,111 or 4,717.",
         "S6 crosswind scenarios")

# ------------------------------------------------------------------ reflections
x = R(x, "the mean estimate increased to 2,223 metric tons/yr (95% CI: 246, 4,201), compared "
         "with 1,036 metric tons/yr (-8, 2,080) when reflections were included.",
         "the mean estimate increased to 2,223 metric tons/yr, compared with 1,036 metric "
         "tons/yr when reflections were included.",
         "S6 reflections scenario")

open(P, "w", encoding="utf-8").write(x)
print("document.xml rewritten (%d bytes)" % len(x))
