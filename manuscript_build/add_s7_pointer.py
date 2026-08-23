"""Add one red sentence at the end of manuscript Section 3.3 pointing to the
new SI Section S7 (screening cumulative noncancer hazard assessment)."""
import redfix

P = "unpacked/word/document.xml"
d = open(P, encoding="utf-8").read()

old = "shows no correlation between them."
new = ("shows no correlation between them. Beyond cancer risk, a screening "
       "cumulative noncancer hazard assessment of all six measured pollutants "
       "(SI Section S7) finds organ-system hazard indices below 1 for "
       "neurological and hematological effects but at or above 1 for endocrine "
       "effects (hydrogen cyanide) and, in the most-exposed blocks, respiratory "
       "effects (hydrogen sulfide), underscoring that a single-pollutant, "
       "single-endpoint view can understate the cumulative burden in this "
       "community.")
assert redfix.count(d, old) == 1
d = redfix.red_replace(d, old, new, "S7 pointer sentence, end of 3.3")

open(P, "w", encoding="utf-8").write(d)
print("document.xml rewritten (%d bytes)" % len(d))
