"""Swap in the redesigned Figure 1 and rewrite its caption.

42_figure1_sampling_density.R -- headed "FIGURE 1 (redesigned per Reviewer 1)" --
produces FinalFig/Figure1_sampling_density.png, 6300 x 3240 px, a two-panel
500 m sampling-density map. The manuscript still carried the originally
submitted image (word/media/image3.jpg, 2048 x 1448) and a caption describing
that older route map, because the script sits in no MAKE_FIGURES group and the
re-run never regenerated it.

The figure is content-current despite its 2026-08-17 timestamp:
figure1_cell_counts_by_route.csv sums to 2,602,928 across its 924 cells, exactly
the 1-s record count this run produces. Group J3 has been added to
MAKE_FIGURES.R so it regenerates from here on.

Panel and overlay definitions read from the script rather than assumed:
  facet A = "Suncor & Phillips 66 route", B = "Sinclair Terminal route" (l.56-58)
  fill    = total 1-s measurements per cell, log10 (l.109)
  shapes  = triangle La Casa, square covered facilities, diamond EPA AQS (l.115-118)
  basemap = CARTO Positron (l.130)

New aspect is 1.9444, so at the 6.5 in text width the box becomes 6.50 x 3.34 in
rather than 6.50 x 4.60. The part is renamed .png and its relationship
retargeted rather than filling a .jpg with PNG bytes.

Run from /tmp/msfix with unpacked/ present.
"""
import os, re, shutil, struct, sys

UN  = "unpacked"
SRC = "/mnt/user-data/uploads/Suncor/FinalFig/Figure1_sampling_density.png"
RID = "rId29"
OLD = "word/media/image3.jpg"
NEW = "word/media/image3.png"
EMU = 914400
TEXT_W = int(6.5 * EMU)

import redfix
from redfix import red_replace as R

b = open(SRC, "rb").read(33)
assert b[:8] == b"\x89PNG\r\n\x1a\n", "source is not a PNG"
w, h = struct.unpack(">II", b[16:24])

doc_path = os.path.join(UN, "word/document.xml")
doc = open(doc_path, encoding="utf-8").read()

# the drawing that carries rId29
i = doc.find('r:embed="%s"' % RID)
assert i > 0, "rId29 not found"
s = doc.rfind("<w:drawing>", 0, i)
e = doc.find("</w:drawing>", i) + len("</w:drawing>")
dr = doc[s:e]
ocx, ocy = (int(v) for v in re.search(r'<wp:extent cx="(\d+)" cy="(\d+)"/>', dr).groups())

cx = TEXT_W
cy = round(cx / (w / h))
dr = re.sub(r'<wp:extent cx="\d+" cy="\d+"/>', '<wp:extent cx="%d" cy="%d"/>' % (cx, cy), dr)
dr = re.sub(r'<a:ext cx="\d+" cy="\d+"/>',     '<a:ext cx="%d" cy="%d"/>' % (cx, cy), dr)
doc = doc[:s] + dr + doc[e:]

print("  Figure 1  %s -> %s" % (OLD, NEW))
print("      %d x %d px (aspect %.4f)" % (w, h, w / h))
print("      extent %.2f x %.2f in -> %.2f x %.2f in"
      % (ocx / EMU, ocy / EMU, cx / EMU, cy / EMU))

# ---- caption -------------------------------------------------------------
doc = R(doc,
    ": Measurements of air toxics corresponding to the Suncor & Phillips 66 and the "
    "Sinclair Terminal routes. The locations of the stationary (La Casa) site and the "
    "meteorological monitoring stations are also displayed. We note that the La Casa site "
    "is also a wind-monitoring site. Discontinuities in the route corresponds to locations "
    "where measurements were not present/removed due to QA/QC issues.",
    ": Sampling density along the two mobile monitoring routes. Panels show the 500 m "
    "analysis grid for (A) the Suncor and Phillips 66 route and (B) the Sinclair Terminal "
    "route, with each cell coloured by the total number of one-second measurements "
    "collected in it over the campaign, on a log scale. Overlaid symbols mark the "
    "stationary measurement site at La Casa (triangle), the covered facilities designated "
    "under HB21-1189 (squares) and the EPA AQS wind monitoring sites (diamonds); the La "
    "Casa site also serves as a wind monitoring site. Gaps in the grid are locations where "
    "no measurement was retained, either because the route was not driven there or because "
    "the measurements were removed by QA/QC. Basemap: CARTO Positron.",
    "Figure 1 caption")

open(doc_path, "w", encoding="utf-8").write(doc)

# ---- part + relationship -------------------------------------------------
rels_path = os.path.join(UN, "word/_rels/document.xml.rels")
rels = open(rels_path, encoding="utf-8").read()
assert 'Target="media/image3.jpg"' in rels
rels = rels.replace('Target="media/image3.jpg"', 'Target="media/image3.png"')
open(rels_path, "w", encoding="utf-8").write(rels)

old_abs = os.path.join(UN, OLD)
old_bytes = os.path.getsize(old_abs)
os.remove(old_abs)
shutil.copyfile(SRC, os.path.join(UN, NEW))
print("      %s -> %s bytes" % (f"{old_bytes:,}", f"{os.path.getsize(os.path.join(UN, NEW)):,}"))
print("relationship retargeted; caption rewritten")
