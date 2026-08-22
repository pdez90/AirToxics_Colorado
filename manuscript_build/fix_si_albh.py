"""SI S1.4: the AL/BH row range.

Recomputed from the 58 delivered monthly CSVs (see R_scripts/72_check_s14_qaqc.R;
the pandas twin reproduces Table S3.1's "reported" row exactly for all six
pollutants, which is what validates the replication):

  pollutant          AL/BH rows carrying a value
  Benzene                                      0
  HCN                                        480
  Xylene                                   2,530
  Toluene                                  2,531
  Trimethylbenzene                         2,532
  H2S                                      8,459

So 480-8,459 is right for five of the six, but benzene has none at all -- every
benzene value CDPHE reported survives the null-qualifier rule. The other three
claims in this paragraph verified exactly: BR never carries a value (0 across
all six), 358,048 negative benzene and 627,134 negative H2S survive, and the
MD-at-half-MDL fraction runs 1.0% to 9.0%.
"""
import redfix
from redfix import red_replace as R
P = "unpacked/word/document.xml"
x = open(P, encoding="utf-8").read()
x = R(x, "so the only codes that actually discard a number are AL and BH, between 480 and "
         "8,459 rows per pollutant.",
         "so the only codes that actually discard a number are AL and BH, which carry a "
         "value on between 480 and 8,459 rows for five of the six pollutants and on none at "
         "all for benzene.",
         "S1.4 AL/BH range")
open(P, "w", encoding="utf-8").write(x)
print("done")
