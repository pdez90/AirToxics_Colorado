"""Rebuild SI Table S3.1 entirely from TABLE_S3.1.csv, which 70_table_s31.R
writes from the 58 monthly CSVs plus mobile_wswd.RData.

Nothing in the table is typed here: every cell is read from that CSV. A cell is
coloured red only if its text differs from what the submitted table showed, so a
reviewer sees exactly what moved.

The row labels change because the old ones named steps that do not exist in the
code. "No of measurements after removing additional contaminated data points"
was in fact four hard-coded date windows in 03_checks_flags.R; the new label
names them and the caption lists them.
"""
import csv, os, re, sys

UP  = "/tmp/sifig/unpacked"
CSV = os.path.expanduser("~/mnt/Suncor/TABLE_S3.1.csv")
RED = '<w:color w:val="FF0000"/>'

rows = {r["pollutant"]: r for r in csv.DictReader(open(CSV))}
ORDER = ["Benzene", "Toluene", "Xylene", "Trimethylbenzene", "H2S", "HCN"]
assert list(rows) == ORDER, list(rows)

def i(v):  return format(int(float(v)), ",")
def f2(v): return "{:,.2f}".format(float(v))
def pc(v): return "%.1f%%" % float(v)

SPEC = [
 ("Most common QA/QC qualifier codes",                         lambda r: r["most_common_flags"]),
 ("Measurements reported by CDPHE",                            lambda r: i(r["reported"])),
 ("Retained after QA/QC (null qualifiers voided)",             lambda r: i(r["after_qc"])),
 ("With valid GPS",                                            lambda r: i(r["gps"])),
 ("After the campaign date exclusions (see caption)",          lambda r: i(r["after_excl"])),
 ("In the analysis set (delay-corrected, native-cadence)",     lambda r: i(r["analysis"])),
 ("Sampling days represented",                                 lambda r: i(r["n_days"])),
 ("Measurement window",                                        lambda r: "%s to %s" % (r["first_day"], r["last_day"])),
 ("Distinct reported values",                                  lambda r: i(r["n_unique"])),
 ("% below the audit MDL (Table S1.2, by lab and period)",      lambda r: pc(r["pct_belowMDL"])),
 ("Minimum (ppb)",                                             lambda r: f2(r["min"])),
 ("5th percentile (ppb)",                                      lambda r: f2(r["p5"])),
 ("25th percentile (ppb)",                                     lambda r: f2(r["p25"])),
 ("Median (ppb)",                                              lambda r: f2(r["median"])),
 ("75th percentile (ppb)",                                     lambda r: f2(r["p75"])),
 ("95th percentile (ppb)",                                     lambda r: f2(r["p95"])),
 ("99th percentile (ppb)",                                     lambda r: f2(r["p99"])),
 ("Maximum (ppb)",                                             lambda r: f2(r["max"])),
 ("Mean (sd) (ppb)",                                           lambda r: "%s (%s)" % (f2(r["mean"]), f2(r["sd"]))),
]

# old label -> new label, for deciding which cells actually changed
CARRY = {
 "Most common flags": "Most common QA/QC qualifier codes",
 "No of measurements with valid GPS after flag removal": "With valid GPS",
 "No of measurements after removing additional contaminated data points":
     "After the campaign date exclusions (see caption)",
 "% of valid measurements < MDL": "% below the audit MDL (Table S1.2, by lab and period)",
 "Valid measurements after shifting measurements and removing data without valid "
 "Latitude and Longitude": "In the analysis set (delay-corrected, native-cadence)",
 "Minimum (ppb)": "Minimum (ppb)", "5th percentile (ppb)": "5th percentile (ppb)",
 "25th percentile (ppb)": "25th percentile (ppb)", "Median (ppb)": "Median (ppb)",
 "75th percentile (ppb)": "75th percentile (ppb)", "95th percentile (ppb)": "95th percentile (ppb)",
 "99th percentile (ppb)": "99th percentile (ppb)", "Maximum (ppb)": "Maximum (ppb)",
 "Mean (sd) (ppb)": "Mean (sd) (ppb)",
}


def same_number(was, now):
    """A cell counts as unchanged when it means the same number. The rebuild
    prints every ppb value to 2 dp, so '0.3' -> '0.30' is a formatting change,
    not a result change, and must not be flagged red."""
    if was == now:
        return True
    def num(s):
        s = s.replace(",", "").replace("%", "").strip()
        try:
            return float(s)
        except ValueError:
            return None
    a, b = num(was), num(now)
    if a is None or b is None:
        return False
    dp = len(was.split(".")[1].rstrip("%")) if "." in was else 0
    return round(b, dp) == round(a, dp)

TR = re.compile(r"<w:tr[ >].*?</w:tr>", re.S)
TC = re.compile(r"<w:tc[ >].*?</w:tc>", re.S)

def text_of(x):
    return re.sub(r"<[^>]+>", "",
                  "".join(re.findall(r"<w:t[^>]*>(.*?)</w:t>", x, re.S))).strip()

xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

# "changed" is measured against the SUBMITTED SI, not against the working copy,
# so re-running this script does not turn the whole table red.
import zipfile
ORIG = os.path.expanduser("~/mnt/manuscript/SI_MobileToxics_CDPHE.docx")
oxml = zipfile.ZipFile(ORIG).read("word/document.xml").decode("utf-8")
ocap = oxml.index("Descriptive Statistics of the Mobile Measurements")
ots = oxml.index("<w:tbl>", ocap); ote = oxml.index("</w:tbl>", ots) + len("</w:tbl>")
submitted_rows = TR.findall(oxml[ots:ote])
print("submitted rows: %d" % len(submitted_rows))

cap = xml.index("Descriptive statistics of the mobile measurements") \
      if "Descriptive statistics of the mobile measurements" in xml \
      else xml.index("Descriptive Statistics of the Mobile Measurements")
ts = xml.index("<w:tbl>", cap); te = xml.index("</w:tbl>", ts) + len("</w:tbl>")
tbl = xml[ts:te]
old_rows = TR.findall(tbl)
print("working-copy rows: %d" % len(old_rows))

# what the submitted table showed, keyed by (new label, pollutant)
old_vals = {}
for tr in submitted_rows[1:]:
    cells = TC.findall(tr)
    lab = text_of(cells[0]).replace("&lt;", "<")
    new = CARRY.get(lab)
    if not new:
        print("  (dropped row: %r)" % lab[:60]); continue
    for p, c in zip(ORDER, cells[1:]):
        old_vals[(new, p)] = text_of(c)

header, template = old_rows[0], old_rows[1]
tcs = TC.findall(template)
lab_tc, val_tc = tcs[0], tcs[1]

def rpr_of(tc):
    r = re.search(r"<w:rPr>.*?</w:rPr>", tc, re.S)
    return r.group(0) if r else "<w:rPr/>"

sys.path.insert(0, "/tmp")
from redfix import make_red as redden

def cell(tc_template, txt, red):
    rpr = rpr_of(tc_template)
    if red: rpr = redden(rpr)
    txt = txt.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    shell = TC.match(tc_template).group(0)
    tcpr = re.search(r"<w:tcPr>.*?</w:tcPr>", shell, re.S)
    tcpr = tcpr.group(0) if tcpr else ""
    ppr = ('<w:pPr><w:widowControl w:val="0"/>'
           '<w:spacing w:line="240" w:lineRule="auto"/><w:rPr/></w:pPr>')
    return ('<w:tc>%s<w:p>%s<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r></w:p></w:tc>'
            % (tcpr, ppr, rpr, txt))

new_rows, nred = [], 0
print("\n%-56s %s" % ("row", "cells changed"))
for lab, fn in SPEC:
    changed = []
    cs = [cell(lab_tc, lab, (lab, "__") not in old_vals and lab not in CARRY.values())]
    # the label cell is red when the row itself is new or its wording changed
    lab_is_new = lab not in CARRY.values()
    lab_changed = lab_is_new or lab not in [CARRY[k] for k in CARRY if CARRY[k] == k]
    cs = [cell(lab_tc, lab, lab_changed)]
    for p in ORDER:
        v = fn(rows[p])
        was = old_vals.get((lab, p))
        red = (was is None) or not same_number(was, v)
        if red: changed.append(p)
        nred += red
        cs.append(cell(val_tc, v, red))
    new_rows.append("<w:tr>%s%s</w:tr>"
                    % (re.search(r"<w:trPr>.*?</w:trPr>", template, re.S).group(0)
                       if "<w:trPr>" in template else "", "".join(cs)))
    print("%-56s %s" % (lab[:54], ", ".join(changed) if changed else "(none)"))

head = tbl[:tbl.index(old_rows[0])]
xml = xml[:ts] + head + header + "".join(new_rows) + "</w:tbl>" + xml[te:]
open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\nnew rows: %d (1 header + %d)   red value cells: %d"
      % (1 + len(new_rows), len(new_rows), nred))
