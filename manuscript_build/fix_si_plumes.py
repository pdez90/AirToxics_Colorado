"""SI Section S6: bring the plume-inversion numbers onto the 2026-08-22 re-run.

Sources, all from the run's own outputs:
  WWTP_H2S_plume_step_counts.csv                      (37 candidates -> 15 with
                                                       >= 3 points -> 4 retained)
  WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv     (471, 706, 1,003, 1,964)
  WWTP_H2S_inversion_summary_mean_ci_METRIC_TPY.csv   (per-scenario means/CIs)

Run from /tmp/sifig with unpacked/ present.
"""
import redfix
from redfix import red_replace as R

P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()

# ---- S6.1 plume identification funnel ------------------------------------
x = R(x, "This step yielded 33 candidate plume events",
         "This step yielded 37 candidate plume events",
         "S6 candidate count")
x = R(x, "Requiring at least three plume-flagged observations per event reduced this set "
         "to 13 events",
         "Requiring at least three plume-flagged observations per event reduced this set "
         "to 15 events",
         "S6 >=3-point count")

# ---- Figure S6.2 caption --------------------------------------------------
x = R(x, "all three retained plumes are shown, one per panel, labelled by plume identifier",
         "all four retained plumes are shown, one per panel, labelled by plume identifier",
         "Figure S6.2 caption")

# ---- S6 baseline result ---------------------------------------------------
x = R(x, "Using the 3 retained", "Using the 4 retained", "S6 retained count")
x = R(x, "inferred emission rates were 471, 1,003 and 1,964 metric tons/year, with a mean "
         "of 1,146 metric tons/year (95% CI: -735, 3,027)",
         "inferred emission rates were 471, 706, 1,003 and 1,964 metric tons/year, with a "
         "mean of 1,036 metric tons/year (95% CI: -8, 2,080)",
         "S6 baseline rates")
x = R(x, "With three intercepts the confidence interval spans zero:",
         "With four intercepts the confidence interval still includes zero:",
         "S6 CI framing")
x = R(x, "is a t-interval on three observations",
         "is a t-interval on four observations",
         "S6 t-interval")

# ---- S6 source-height sensitivity ----------------------------------------
x = R(x, "Mean inferred emissions were 1,146 metric tons/yr (95% CI: -735, 3,027) for a 1 m "
         "source height, 1,146 metric tons/yr (-735, 3,027) for 10 m, 1,146 metric tons/yr "
         "(-734, 3,027) for 15 m, 1,146 metric tons/yr (-734, 3,027) for 20 m, 1,147 metric "
         "tons/yr (-733, 3,028) for 30 m, and 1,149 metric tons/yr (-731, 3,030) for 50 m.",
         "Mean inferred emissions were 1,036 metric tons/yr (95% CI: -8, 2,080) for a 1 m "
         "source height, 1,036 metric tons/yr (-8, 2,080) for 10 m, 1,036 metric tons/yr "
         "(-8, 2,080) for 15 m, 1,036 metric tons/yr (-8, 2,081) for 20 m, 1,037 metric "
         "tons/yr (-8, 2,081) for 30 m, and 1,038 metric tons/yr (-6, 2,083) for 50 m.",
         "S6 source-height scenarios")

# ---- S6 reflections -------------------------------------------------------
x = R(x, "Without reflections, the mean estimate increased to 2,304 metric tons/yr (95% CI: "
         "-1,444, 6,053), compared with 1,146 metric tons/yr (-735, 3,027) when reflections "
         "were included.",
         "Without reflections, the mean estimate increased to 2,223 metric tons/yr (95% CI: "
         "246, 4,201), compared with 1,036 metric tons/yr (-8, 2,080) when reflections were "
         "included.",
         "S6 reflections scenario")

# ---- S6 scenario envelope -------------------------------------------------
x = R(x, "mean estimates range from 415 to 2,304 metric tons/yr, and from 415 to 1,774 "
         "across the 25 of them that perturb an uncertain input rather than removing the "
         "reflection term from the model",
         "mean estimates range from 420 to 2,223 metric tons/yr, and from 420 to 1,687 "
         "across the 25 of them that perturb an uncertain input rather than removing the "
         "reflection term from the model",
         "S6 scenario envelope")
x = R(x, ", and each rests on the same three intercepts,",
         ", and each rests on the same four intercepts,",
         "S6 scenario independence")

# ---- Figure S6.5 caption --------------------------------------------------
x = R(x, "The three retained intercepts give 471, 1,003 and 1,964 metric tons/yr,",
         "The four retained intercepts give 471, 706, 1,003 and 1,964 metric tons/yr,",
         "Figure S6.5 caption")

open(P, "w", encoding="utf-8").write(x)
print("document.xml rewritten (%d bytes)" % len(x))

# ---- appended: the remaining S6 sensitivity paragraphs --------------------
x = open(P, encoding="utf-8").read()

# funnel narrative
x = R(x, "all 13 events satisfied the minimum peak enhancement criterion",
         "all 15 events satisfied the minimum peak enhancement criterion",
         "S6.1 peak-enhancement step")
x = R(x, "Shape-based filters removed two events (n = 12 after rise; n = 11 after fall), "
         "while the prominence criterion was the most restrictive, reducing the set to 5 "
         "events. The duration criterion removed one further event.",
         "Shape-based filters removed two events (n = 14 after rise; n = 13 after fall), "
         "while the prominence criterion was the most restrictive, reducing the set to 7 "
         "events. The duration criterion removed two further events.",
         "S6.1 shape/prominence/duration steps")
x = R(x, "it could be evaluated on only 2 of the 13 events",
         "it could be evaluated on only 3 of the 15 events",
         "S6.1 wind-consistency evaluability")

# Figure S6.1 caption
x = R(x, "Of 33 initial plume-flagged events, 13 contained ≥3 plume observations. All 13 "
         "met the minimum peak enhancement criterion (≥1 ppb). Shape-based filters "
         "(edge-defined rise and fall) reduced the sample to 11 events, and the prominence "
         "criterion (≥0.2 ppb) further reduced the set to 5 events. The duration criterion "
         "(10-180 s) removed one further event. Wind-direction consistency (circular SD "
         "≤15°) removed none and was evaluable on 2 of the 13 events, while the atmospheric "
         "stability filter (Pasquill classes B-D) excluded one additional event. In total, "
         "3 plume events passed all filters and were retained for Gaussian inversion.",
         "Of 37 initial plume-flagged events, 15 contained ≥3 plume observations. All 15 met "
         "the minimum peak enhancement criterion (≥1 ppb). Shape-based filters (edge-defined "
         "rise and fall) reduced the sample to 13 events, and the prominence criterion "
         "(≥0.2 ppb) further reduced the set to 7 events. The duration criterion (10-180 s) "
         "removed two further events. Wind-direction consistency (circular SD ≤15°) removed "
         "none and was evaluable on 3 of the 15 events, while the atmospheric stability "
         "filter (Pasquill classes B-D) excluded one additional event. In total, 4 plume "
         "events passed all filters and were retained for Gaussian inversion.",
         "Figure S6.1 caption")

# wind speed
x = R(x, "Reducing wind speed by 20% decreased the mean estimate to 917 metric tons/yr "
         "(95% CI: -588, 2,421), whereas increasing wind speed by 20% increased the "
         "estimate to 1,375 metric tons/yr (-881, 3,632).",
         "Reducing wind speed by 20% decreased the mean estimate to 829 metric tons/yr "
         "(95% CI: -6, 1,664), whereas increasing wind speed by 20% increased the estimate "
         "to 1,243 metric tons/yr (-10, 2,496).",
         "S6 wind-speed scenarios")

# stability
x = R(x, "increased the mean emission estimate to 1,458 metric tons/yr (95% CI: 27, 2,889), "
         "whereas shifting toward more unstable conditions (CA",
         "increased the mean emission estimate to 1,314 metric tons/yr (95% CI: 438, 2,191), "
         "whereas shifting toward more unstable conditions (CA",
         "S6 stability CAT-1")
x = R(x, "reduced the estimate to 415 metric tons/yr (-267, 1,096).",
         "reduced the estimate to 420 metric tons/yr (64, 777).",
         "S6 stability CAT+1")

# downwind distance
x = R(x, "decreased the mean estimate to 961 metric tons/yr (95% CI: -493, 2,415), while "
         "increasing the distance by 10% increased the estimate to 1,345 metric tons/yr "
         "(-1,028, 3,719). This corresponds to changes on the order of roughly -16% to +17% "
         "relative to the baseline estimate.",
         "decreased the mean estimate to 891 metric tons/yr (95% CI: 98, 1,683), while "
         "increasing the distance by 10% increased the estimate to 1,191 metric tons/yr "
         "(-144, 2,526). This corresponds to changes on the order of roughly -14% to +15% "
         "relative to the baseline estimate.",
         "S6 distance scenarios")

# crosswind geometry
x = R(x, "gives 1,030 metric tons/yr (95% CI: -951, 3,011); shifting the bearing by 5 "
         "degrees gives 1,045 (-945, 3,036) or 1,774 (223, 3,325) depending on direction, "
         "and by 10 degrees 1,279 (-656, 3,214) or 4,793 (251, 9,334).",
         "gives 905 metric tons/yr (95% CI: -205, 2,015); shifting the bearing by 5 degrees "
         "gives 917 (-201, 2,035) or 1,687 (829, 2,544) depending on direction, and by 10 "
         "degrees 1,111 (-34, 2,256) or 4,717 (2,329, 7,104).",
         "S6 crosswind scenarios")

# averaging time
x = R(x, "gives 879 metric tons/yr at 60 s and 1,547 at 3,600 s, against 1,146 at the 600 s "
         "reference",
         "gives 833 metric tons/yr at 60 s and 1,378 at 3,600 s, against 1,036 at the 600 s "
         "reference",
         "S6 averaging-time scenarios")

open(P, "w", encoding="utf-8").write(x)
print("sensitivity paragraphs updated")

# ---- appended: the per-event distance/rate paragraph ----------------------
# baseline_H_12.2m rows: plume 10 @1.95 km 1,964; 13 @3.55 km 471;
#                        2 @4.11 km 706; 32 @4.30 km 1,003
x = open(P, encoding="utf-8").read()
x = R(x, "at 1.95, 3.55 and 4.30 km", "at 1.95, 3.55, 4.11 and 4.30 km",
         "S6 event distances")
x = R(x, "The January 2024 and May 2024 events gave 471 and 1,003 metric tons/yr "
         "respectively, while the November 2023 event produced a notably larger inferred "
         "rate of 1,964 metric tons/yr.",
         "The January 2024, May 2024 and April 2023 events gave 471, 1,003 and 706 metric "
         "tons/yr respectively, while the November 2023 event, intercepted closest to the "
         "facility at 1.95 km, produced a notably larger inferred rate of 1,964 metric "
         "tons/yr.",
         "S6 per-event rates")
open(P, "w", encoding="utf-8").write(x)
print("per-event paragraph updated")
