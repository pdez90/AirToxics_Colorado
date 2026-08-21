# tests/

Three scripts. The first two are regression tests and should pass on any
checkout; the third is a diagnostic that reports numbers rather than passing or
failing.

```bash
Rscript tests/test_p08_geometry.R
SUNCOR_CSV_DIR=Updated/csv SUNCOR_BASE=~/Downloads/Suncor Rscript tests/test_time_convention.R
SUNCOR_BASE=~/Downloads/Suncor Rscript tests/impact_of_time_fixes.R
```

Both tests exit non-zero on failure, so they drop straight into CI.

## test_time_convention.R

Locks down the one unusual thing about time here: **`date` is a fixed-MST wall
clock stored with a UTC attribute — not an absolute UTC instant.** The tzone
attribute is a carrier for the clock reading, not a claim about the instant.

Four sections:

1. **Unit test.** `09:00` Local_Time_MST must map to `16:00` UTC in *both*
   seasons under `force_tz("MST")`; under `force_tz("America/Denver")` it maps
   to `15:00` in summer and `16:00` in winter. That asymmetry is the whole
   point — America/Denver agrees in winter, which is what makes the summer
   error easy to miss. Needs no data.
2. **Raw-data assertion.** Every `Local_Time_MST` string carries `-0700`, in all
   12 calendar months. Mirrors the assertion inside `02_newmobile_data.R` so the
   suite catches a changed data delivery even if nobody re-runs 02.
3. **DST diagnostic.** Offsets could in principle be mislabelled, which section
   2 would not catch. This tests the labels against behaviour instead: crews
   start at a fixed *civil* hour, so on a true-MST clock the day's first record
   must fall about an hour earlier during daylight-saving months. Measured over
   the 101 sampling days it does, by **0.95 h (95% CI 0.60–1.30)** —
   consistent with 1.00 h (p = 0.77), and 0.00 h rejected (p ≈ 1e-6). Also
   confirms no sampling day falls on a DST transition date.
4. **Intermediates.** Any `.RData` on disk still carries the convention.
5. **Static scan.** No live code converts pipeline timestamps via
   `America/Denver`. Comments and `stop()` messages naming the zone are
   ignored; only conversions count. This is the mechanical enforcement — the
   convention has now been re-broken twice by code that looked locally
   reasonable (`36_hysplit`, and `H04`'s helper default plus its example call),
   so a scan is worth more than another comment. Verified against a negative
   control: reintroducing `tz_local = "America/Denver"` makes the test fail and
   names the file and line. Also checks that P04 and H04 derive the same HRRR
   hour.

Sections 2–4 skip cleanly (5 always runs) when the data isn't present, so the test is still
useful on a bare checkout.

## test_p08_geometry.R

Closed-loop test of the plume geometry, not a re-statement of the maths. It
builds a plume forward from a source of known strength, places the receptor off
the centreline the way the ±10° acceptance window actually does, computes the
concentration that receptor would see, then hands it to P08's own
`invert_gaussian()` and checks the known source strength comes back.

540 cases — 4 distances × 5 off-axis angles × 3 stability classes × 3 wind
speeds × 3 boundary-layer heights. Recovery is exact to **7e-14 %**. The forward
model is written independently in the test file, so this is not the code
checking itself.

It also measures what the superseded centreline assumption (θ forced to 0) does
to the same data: **median −19.6 %, worst −80.3 %** — an under-estimate, the
direction the method note claims. And it confirms `y/σy` stays under the
`ILL_SIGY = 2` flag everywhere the baseline acceptance window admits (max 1.81),
so the scenarios that *do* trip the flag are the wd±10 and averaging-time axes
that shrink σy.

## impact_of_time_fixes.R

Not a test — it quantifies what the time-convention work changed in the
**outputs**, from the intermediates already on disk, with no re-run. As of
2026-08-21 it reports:

- **Scripts 02 / 06** — the intermediates were already built on the corrected
  convention, so the parse change moves no existing number. Its value is the
  guard against a future re-run silently switching conventions.
- **Script 06** — 2,327,586 of 2,602,928 rows (89.4%) have matched station
  wind, and every matched record uses the wind hour that contains it (lag −1 to
  0 h). Under the America/Denver parse that lag would be 6–7 hours.
- **Script 36** — the one substantive change. 35 of the 60 HYSPLIT trajectories
  (58%) were launched an hour early. The receptor *set* is unchanged (729
  receptor-hours either way), so this is a re-run of 36, not a change of method.
  Anything built on those back-trajectories should be regenerated.
