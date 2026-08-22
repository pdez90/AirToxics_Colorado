"""Rebuild Table S5.1 from the 2026-08-21 run.

As submitted the table had 17 rows (one per hotspot group of the old run), no
group identifiers, and only 3 of the 4 panels its own caption promises. This
rebuild produces 18 rows -- one per persistent group in
MASTER_hotspot_group_index.csv -- each naming its group id and carrying all four
panels the caption describes:

  (i)   group_<id>_map_local.png      100 m buffer map
  (ii)  group_<id>_map_regional.png   regional context
  (iii) group_<id>_pairwise_wd.png    wind-coloured pairwise scatterplots
  (iv)  group_<id>_polar.png          non-weighted concentration frequency

Exceedance-day counts come from group_<id>_pollutant_highday_table.csv
(column n_days_high); the nearest TRI facility comes from
MASTER_hotspot_group_index_with_TRI.csv. Nothing is typed by hand.

All new text is red (FF0000), consistent with the revision convention.
"""
import csv, os, re, sys
from PIL import Image
Image.MAX_IMAGE_PIXELS = None

UP   = "/tmp/sifig/unpacked"
REP  = os.path.expanduser("~/mnt/Suncor/hotspot_group_reports")
EMU  = 914400
PANEL_W = int(2.78 * EMU)      # two panels across the 5.87 in right-hand column
LONG_PX = 1150                 # embedded resolution per panel
RID0    = 2000                 # new relationship ids start here
IMG0    = 200                  # new media part numbers start here

PRETTY = {"benzene": "benzene", "toluene": "toluene",
          "trimethylbenzene": "trimethylbenzene", "xylene": "xylene",
          "h2s": "H2S", "hcn": "HCN"}
SUBSCRIPT_OF = {"H2S": ("H", "2", "S")}
ORDER = ["benzene", "toluene", "trimethylbenzene", "xylene", "h2s", "hcn"]


def titlecase_facility(name):
    small = {"of", "and", "the", "&"}
    out = []
    for w in name.split():
        if w in ("LLC", "INC", "CO", "US", "EJ", "AR", "KBP", "B&B", "&"):
            out.append(w)
        elif w.lower() in small:
            out.append(w.lower())
        else:
            out.append(w.capitalize())
    return " ".join(out)


# ---- data ---------------------------------------------------------------
idx = list(csv.DictReader(open(os.path.join(REP, "MASTER_hotspot_group_index_with_TRI.csv"))))
groups = []
for r in idx:
    gid = r["group_id"]
    days = {}
    with open(os.path.join(REP, "group_%s_pollutant_highday_table.csv" % gid)) as fh:
        for d in csv.DictReader(fh):
            days[d["pollutant"]] = int(d["n_days_high"])
    panels = []
    for suffix in ("map_local", "map_regional", "pairwise_wd", "polar"):
        p = os.path.join(REP, "group_%s_%s.png" % (gid, suffix))
        if not os.path.exists(p):
            sys.exit("missing panel: " + p)
        panels.append(p)
    groups.append(dict(gid=gid, days=days, panels=panels,
                       polls=r["pollutants_in_group_summary"].split("+"),
                       npoll=int(r["n_pollutants_in_group_summary"]),
                       tri=titlecase_facility(r["tri_name"]),
                       trikm=float(r["tri_dist_km"])))
print("groups: %d   panels: %d" % (len(groups), 4 * len(groups)))

# ---- XML helpers --------------------------------------------------------
RED = '<w:color w:val="FF0000"/>'


def run(text, bold=False, sub=False):
    rpr = "<w:rPr>"
    if bold:
        rpr += '<w:b w:val="1"/><w:bCs w:val="1"/>'
    rpr += RED
    if sub:
        rpr += '<w:vertAlign w:val="subscript"/>'
    rpr += '<w:rtl w:val="0"/></w:rPr>'
    text = (text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"))
    return '<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>' % (rpr, text)


def runs_with_h2s(text, bold=False):
    """Emit H2S with a real subscript 2; everything else plain."""
    out, i = [], 0
    for m in re.finditer(r"H2S", text):
        if m.start() > i:
            out.append(run(text[i:m.start()], bold))
        out.append(run("H", bold))
        out.append(run("2", bold, sub=True))
        out.append(run("S", bold))
        i = m.end()
    if i < len(text):
        out.append(run(text[i:], bold))
    return "".join(out)


PPR = ('<w:pPr><w:widowControl w:val="0"/>'
       '<w:spacing w:line="240" w:lineRule="auto"/><w:rPr/></w:pPr>')
PPR_C = ('<w:pPr><w:widowControl w:val="0"/><w:jc w:val="center"/>'
         '<w:spacing w:line="240" w:lineRule="auto"/><w:rPr/></w:pPr>')
TCPR = ('<w:tcPr><w:shd w:fill="auto" w:val="clear"/><w:tcMar>'
        '<w:top w:w="100.0" w:type="dxa"/><w:left w:w="100.0" w:type="dxa"/>'
        '<w:bottom w:w="100.0" w:type="dxa"/><w:right w:w="100.0" w:type="dxa"/>'
        '</w:tcMar><w:vAlign w:val="top"/></w:tcPr>')


def drawing(rid, name, cx, cy, docpr):
    return (
        '<w:drawing><wp:inline distB="0" distT="0" distL="0" distR="0">'
        '<wp:extent cx="%d" cy="%d"/><wp:effectExtent b="0" l="0" r="0" t="0"/>'
        '<wp:docPr id="%d" name="%s"/><a:graphic><a:graphicData '
        'uri="http://schemas.openxmlformats.org/drawingml/2006/picture">'
        '<pic:pic><pic:nvPicPr><pic:cNvPr id="0" name="%s"/>'
        '<pic:cNvPicPr preferRelativeResize="0"/></pic:nvPicPr><pic:blipFill>'
        '<a:blip r:embed="%s"/><a:srcRect b="0" l="0" r="0" t="0"/>'
        '<a:stretch><a:fillRect/></a:stretch></pic:blipFill><pic:spPr>'
        '<a:xfrm><a:off x="0" y="0"/><a:ext cx="%d" cy="%d"/></a:xfrm>'
        '<a:prstGeom prst="rect"/><a:ln/></pic:spPr></pic:pic>'
        '</a:graphicData></a:graphic></wp:inline></w:drawing>'
        % (cx, cy, docpr, name, name, rid, cx, cy))


# ---- write the new media parts -----------------------------------------
rid_n, img_n, docpr_n = RID0, IMG0, 9000
new_rels, media_written = [], []
for g in groups:
    g["embeds"] = []
    for p in g["panels"]:
        im = Image.open(p)
        w0, h0 = im.size
        sc = min(1.0, LONG_PX / max(w0, h0))
        nw, nh = round(w0 * sc), round(h0 * sc)
        part = "image%d.png" % img_n
        im.convert("RGBA").resize((nw, nh), Image.LANCZOS).save(
            os.path.join(UP, "word/media", part), "PNG", optimize=True)
        media_written.append(part)
        rid = "rId%d" % rid_n
        new_rels.append((rid, part))
        cx = PANEL_W
        cy = round(cx * h0 / w0)
        g["embeds"].append(drawing(rid, part, cx, cy, docpr_n))
        rid_n += 1; img_n += 1; docpr_n += 1
print("media parts written: %d" % len(media_written))

# ---- build the rows -----------------------------------------------------
rows_xml = []
for g in groups:
    polls = ", ".join(PRETTY[p] for p in ORDER if p in g["polls"])
    dparts = ["%s %d" % (PRETTY[p], g["days"][p])
              for p in ORDER if g["days"].get(p, 0) > 0]
    cap1 = "Group %s - %d pollutants (%s)." % (g["gid"], g["npoll"], polls)
    cap2 = "Days above the campaign 99th percentile within 100 m: %s." % "; ".join(dparts)
    cap3 = "Nearest TRI facility: %s (%.2f km)." % (g["tri"], g["trikm"])

    left = ('<w:tc>%s'
            '<w:p>%s%s</w:p>'
            '<w:p>%s%s</w:p>'
            '<w:p>%s%s</w:p>'
            '</w:tc>' % (TCPR,
                         PPR, runs_with_h2s(cap1, bold=True),
                         PPR, runs_with_h2s(cap2),
                         PPR, runs_with_h2s(cap3)))
    e = g["embeds"]
    right = ('<w:tc>%s'
             '<w:p>%s<w:r><w:rPr><w:rtl w:val="0"/></w:rPr>%s%s</w:r></w:p>'
             '<w:p>%s<w:r><w:rPr><w:rtl w:val="0"/></w:rPr>%s%s</w:r></w:p>'
             '</w:tc>' % (TCPR, PPR_C, e[0], e[1], PPR_C, e[2], e[3]))
    rows_xml.append('<w:tr><w:trPr><w:cantSplit w:val="0"/>'
                    '<w:tblHeader w:val="0"/></w:trPr>%s%s</w:tr>' % (left, right))

# ---- splice into document.xml ------------------------------------------
xml = open("%s/word/document.xml" % UP, encoding="utf-8").read()
i = xml.index("Table S5.1")
ts = xml.index("<w:tbl>", i)
te = xml.index("</w:tbl>", ts) + len("</w:tbl>")
tbl = xml[ts:te]
old_rows = re.findall(r"<w:tr[ >].*?</w:tr>", tbl, re.S)
old_rids = sorted(set(re.findall(r'r:embed="(rId\d+)"', tbl)),
                  key=lambda s: int(s[3:]))
print("old rows: %d   old embeds: %d" % (len(old_rows), len(old_rids)))

head = tbl[:tbl.index(old_rows[0])]
new_tbl = head + old_rows[0] + "".join(rows_xml) + "</w:tbl>"
xml = xml[:ts] + new_tbl + xml[te:]
print("new rows: %d (1 header + %d groups)" % (1 + len(rows_xml), len(rows_xml)))

# ---- relationships ------------------------------------------------------
rp = "%s/word/_rels/document.xml.rels" % UP
rels = open(rp, encoding="utf-8").read()
rmap = dict(re.findall(r'Id="(rId\d+)"[^>]*Target="([^"]+)"', rels))
dead_parts = [rmap[r] for r in old_rids if r in rmap]

for r in old_rids:
    rels = re.sub(r'<Relationship Id="%s"[^>]*/>' % r, "", rels)
add = "".join(
    '<Relationship Id="%s" Type="http://schemas.openxmlformats.org/'
    'officeDocument/2006/relationships/image" Target="media/%s"/>' % (rid, part)
    for rid, part in new_rels)
rels = rels.replace("</Relationships>", add + "</Relationships>")
open(rp, "w", encoding="utf-8").write(rels)
print("rels: removed %d, added %d" % (len(old_rids), len(new_rels)))

# ---- drop the now-unreferenced media parts ------------------------------
still = set(re.findall(r'Target="media/([^"]+)"', rels))
removed = 0
for part in dead_parts:
    base = part.split("/")[-1]
    if base in still:
        continue
    p = os.path.join(UP, "word/media", base)
    if os.path.exists(p):
        os.remove(p); removed += 1
print("orphan media parts deleted: %d" % removed)

open("%s/word/document.xml" % UP, "w", encoding="utf-8").write(xml)
print("document.xml rewritten (%d bytes)" % len(xml.encode()))
