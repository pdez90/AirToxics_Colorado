"""SI S4: justify the 500 m grid from the measured driving speeds.

The submitted sentence gave "30-60 km/h" for the mobile laboratories. Computed
from the same GPS segments that build Figure S3.1 -- consecutive fixes with a
gap of 1-5 s, implausible speeds above 130 km/h dropped, Goodrich excluded --
over 2,596,958 segments:

    median 26.0 km/h, IQR 13.8-38.4, 90th 57.7, 95th 73.8, max 130
    13% of segments are stopped (<= 1 km/h); moving-only median 28.5
    only 32% of segments fall inside 30-60 km/h

So 30-60 overstates the typical speed, and Figure S3.1 in the same document now
displays per-run averages with a median of 29.2 km/h, which a reader can compare
against it. The replacement states the measured distribution and makes the
sampling argument explicitly, which supports the 500 m choice more directly than
a speed range does: at the 95th percentile a 1-s sample advances 21 m and a
500 m cell takes ~24 s to cross.

Run from /tmp/sifig with unpacked/ present.
"""
import redfix
from redfix import red_replace as R

P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()

x = R(x, "This cell size accommodates the fast road speeds of the mobile laboratories "
         "(30-60 km/h, dictated by roads and traffic), while still providing "
         "high-resolution information.",
         "This cell size accommodates the speeds at which the mobile laboratories travel. "
         "Across the 2,596,958 GPS segments underlying Figure S3.1, the median speed "
         "between consecutive fixes was 26 km/h (interquartile range 14-38 km/h, 95th "
         "percentile 74 km/h), with 13% of seconds stationary. Even at the 95th percentile "
         "a one-second sample advances only 21 m and a 500 m cell takes roughly 24 s to "
         "cross, so each cell accumulates many samples per pass while still providing "
         "high-resolution information.",
         "S4 grid-size justification")

open(P, "w", encoding="utf-8").write(x)
print("document.xml rewritten (%d bytes)" % len(x))
