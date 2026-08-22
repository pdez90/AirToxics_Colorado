"""Replace Table S1.1's Measurement Period row with the periods the measurement
files actually cover, read from TABLE_S1.1_periods.csv (written by
R_scripts/71_table_s11.R).

Checked against the data, the submitted row was wrong in three of three cells:
  mobile        Feb 26 2023 - Mar 13 2025   ->  Feb 16 2023 - Jun 23 2025
  Vocus 2R      Jun 26 - Aug 2 2024         ->  Jun 24 - Aug 2 2024
  Vocus Elf     Jun 2 - Sep 8 2023          ->  Jun 2 - Aug 8 2023
                (the winter leg, Dec 20 2023 - Feb 20 2024, was right)
"""
import csv, os, re, sys
sys.path.insert(0, "/tmp")
from redfix import make_red

UP  = "/tmp/sifig/unpacked"
CSV = os.path.expanduser("~/mnt/Suncor/TABLE_S1.1_periods.csv")
rows = {r["deployment"]: r for r in csv.DictReader(open(CSV))}

MON = ["January","February","March","April","May","June",
       "July","August","September","October","November","December"]
def pretty(iso):
    y, m, d = iso.split("-")
    return "%s %d, %s" % (MON[int(m)-1], int(d), y)
def rng(dep):
    r = rows[dep]
    return "%s - %s" % (pretty(r["first_record"]), pretty(r["last_record"]))

CELLS = [
    ("mobile (Vocus Eiger)", rng("CDPHE vans, both")),
    ("Vocus 2R, La Casa",    rng("La Casa, summer 2024")),
    ("Vocus Elf, La Casa",   "%s and %s" % (rng("La Casa, summer 2023"),
                                            rng("La Casa, winter 2023/24"))),
]

TR = re.compile(r"<w:tr[ >].*?</w:tr>", re.S)
TC = re.compile(r"<w:tc[ >].*?</w:tc>", re.S)
def text_of(x):
    return re.sub(r"<[^>]+>", "", "".join(
        re.findall(r"<w:t[^>]*>(.*?)</w:t>", x, re.S))).strip()

xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()
cap = xml.index("Approximate resolution, limit of detection")
ts = xml.index("<w:tbl>", cap); te = xml.index("</w:tbl>", ts) + len("</w:tbl>")
tbl = xml[ts:te]

target = None
for tr in TR.findall(tbl):
    cells = TC.findall(tr)
    if cells and text_of(cells[0]).strip().lower().startswith("measurement period"):
        target = (tr, cells); break
if target is None:
    raise SystemExit("Measurement Period row not found")
tr, cells = target
assert len(cells) == 4, "expected 4 cells, got %d" % len(cells)

newtr = tr
for i, (label, txt) in enumerate(CELLS, start=1):
    was = text_of(cells[i])
    tcpr = re.search(r"<w:tcPr>.*?</w:tcPr>", cells[i], re.S)
    tcpr = tcpr.group(0) if tcpr else ""
    rpr = re.search(r"<w:rPr>.*?</w:rPr>", cells[i], re.S)
    rpr = make_red(rpr.group(0) if rpr else "<w:rPr/>")
    ppr = ('<w:pPr><w:widowControl w:val="0"/>'
           '<w:spacing w:line="240" w:lineRule="auto"/><w:rPr/></w:pPr>')
    body = ('<w:p>%s<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r></w:p>'
            % (ppr, rpr, txt.replace("&", "&amp;")))
    newtr = newtr.replace(cells[i], "<w:tc>%s%s</w:tc>" % (tcpr, body), 1)
    print("  %-22s\n      was: %s\n      now: %s" % (label, was, txt))

tbl = tbl.replace(tr, newtr, 1)
xml = xml[:ts] + tbl + xml[te:]
open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\nMeasurement Period row rewritten from TABLE_S1.1_periods.csv")
