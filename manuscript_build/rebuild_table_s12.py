"""Rebuild SI Table S1.2 (audit MDLs) from CDPHE_audit_MDLs.csv, which
69_cdphe_audit_mdls.R reads out of the "Quarterly Summary" tab of every
quarterly packet - the location the HB21-1189 read-me PDFs name as the source
of record for method detection limits.

Checking the submitted table against that source found:
  * EMU hydrogen cyanide listed as 0.18 ppbV for Jan-Mar 2025. The packet says
    18.0. A misplaced decimal point, and the reason the below-MDL fraction for
    HCN looked implausible.
  * EMU hydrogen cyanide left blank before 2025. The packets give 5.0 ppbV from
    2023 Q4.
  * No values at all for 2025 Q2, though sampling runs to 23 June 2025.
  * EMU trimethylbenzene's 0.44 period ended at September 2024; it runs through
    December 2024.
Everything else in the submitted table matches the packets exactly.
"""
import csv, os, re, sys
sys.path.insert(0, "/tmp")
from redfix import make_red

UP  = "/tmp/sifig/unpacked"
CSV = os.path.expanduser("~/mnt/Suncor/CDPHE_audit_MDLs.csv")

MON = ["January","February","March","April","May","June",
       "July","August","September","October","November","December"]
Q_START = {1: 0, 2: 3, 3: 6, 4: 9}
Q_END   = {1: 2, 2: 5, 3: 8, 4: 11}
LAST_Q  = (2025, 2)          # sampling ends 23 June 2025

rows = list(csv.DictReader(open(CSV)))
ORDER = ["Benzene", "Toluene", "Xylene", "Trimethylbenzene",
         "Hydrogen sulfide (H2S)", "Hydrogen cyanide (HCN)"]


def periods(compound, col):
    """Collapse consecutive quarters carrying the same MDL into one label."""
    d = sorted((int(r["year"]), int(r["quarter"]), r[col])
               for r in rows if r["compound"] == compound)
    out, run = [], None
    for y, q, v in d:
        v = v.strip()
        if not v:
            if run: out.append(run); run = None
            continue
        if run and run[2] == v:
            run = (run[0], (y, q), v)
        else:
            if run: out.append(run)
            run = ((y, q), (y, q), v)
    if run: out.append(run)
    txt = []
    for (y0, q0), (y1, q1), v in out:
        a = "%s %d" % (MON[Q_START[q0]], y0)
        b = "%s %d" % (MON[Q_END[q1]], y1)
        if (y1, q1) == LAST_Q:
            b = "June 2025"                      # campaign ends 23 June 2025
        txt.append("%s - %s: %s" % (a, b, v) if a != b else "%s: %s" % (a, v))
    return txt


TR = re.compile(r"<w:tr[ >].*?</w:tr>", re.S)
TC = re.compile(r"<w:tc[ >].*?</w:tc>", re.S)

def text_of(x):
    return re.sub(r"<[^>]+>", "",
                  " | ".join(re.sub(r"<[^>]+>", "", "".join(
                      re.findall(r"<w:t[^>]*>(.*?)</w:t>", p, re.S)))
                      for p in re.findall(r"<w:p[ >].*?</w:p>", x, re.S))).strip()

xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()
cap = xml.index("Method Detection Limits and LODs for the CAT and EMU labs")
ts = xml.index("<w:tbl>", cap); te = xml.index("</w:tbl>", ts) + len("</w:tbl>")
tbl = xml[ts:te]
trs = TR.findall(tbl)
print("Table S1.2 rows: %d" % len(trs))

nred = 0
for tr in trs[1:]:
    cells = TC.findall(tr)
    if len(cells) < 4:
        continue
    compound = text_of(cells[1]).strip()
    match = [c for c in ORDER if c.lower().startswith(compound.lower().split(" (")[0])]
    if not match:
        print("  (no source rows for %r)" % compound); continue
    comp = match[0]
    newtr = tr
    for ci, col in ((2, "cat_mdl"), (3, "emu_mdl")):
        want = periods(comp, col)
        was = text_of(cells[ci])
        now = " | ".join(want)
        if not want:
            continue
        changed = re.sub(r"\s+", "", was) != re.sub(r"\s+", "", now)
        nred += changed
        # keep the cell shell, rebuild its paragraphs
        tcpr = re.search(r"<w:tcPr>.*?</w:tcPr>", cells[ci], re.S)
        tcpr = tcpr.group(0) if tcpr else ""
        rpr = re.search(r"<w:rPr>.*?</w:rPr>", cells[ci], re.S)
        rpr = rpr.group(0) if rpr else "<w:rPr/>"
        if changed:
            rpr = make_red(rpr)
        ppr = ('<w:pPr><w:widowControl w:val="0"/>'
               '<w:spacing w:line="240" w:lineRule="auto"/><w:rPr/></w:pPr>')
        body = "".join('<w:p>%s<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r></w:p>'
                       % (ppr, rpr, t.replace("&", "&amp;")) for t in want)
        newtr = newtr.replace(cells[ci], "<w:tc>%s%s</w:tc>" % (tcpr, body), 1)
        print("  %-24s %-8s %s" % (compound[:24], col[:3].upper(),
                                   "CHANGED" if changed else "same"))
        if changed:
            print("      was: %s" % was)
            print("      now: %s" % now)
    tbl = tbl.replace(tr, newtr, 1)

xml = xml[:ts] + tbl + xml[te:]
open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\ncells changed: %d" % nred)
