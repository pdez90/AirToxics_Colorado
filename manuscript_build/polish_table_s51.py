"""Two fixes to the rebuilt Table S5.1.

1. cantSplit -- each group's row must stay on one page, otherwise a panel pair
   floats onto the next page with an empty caption cell beside it.
2. facility-name casing -- TRI names arrive in the source CSV in all caps;
   the title-caser was leaving CO/INC uppercase and mangling hyphens.
"""
import re
UP = "/tmp/sifig/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

i  = xml.index("Table S5.1")
ts = xml.index("<w:tbl>", i)
te = xml.index("</w:tbl>", ts) + len("</w:tbl>")
tbl = xml[ts:te]

n = tbl.count('<w:cantSplit w:val="0"/>')
tbl = tbl.replace('<w:cantSplit w:val="0"/>', '<w:cantSplit w:val="1"/>')
print("rows pinned to one page:", n)

FIX = {
    "Brannan Sand & Gravel- Central Ready Mix": "Brannan Sand & Gravel – Central Ready Mix",
    "Phillips 66 CO Denver Terminal":           "Phillips 66 Co. Denver Terminal",
    "A R Wilfley & Sons INC":                   "A. R. Wilfley & Sons Inc.",
    "B&amp;B Blending LLC":                     "B&amp;B Blending LLC",
    "Kbp Coil Coaters INC":                     "KBP Coil Coaters Inc.",
    "Sika Mbcc US LLC":                         "Sika MBCC US LLC",
    "Rocky Mountain Bottle CO":                 "Rocky Mountain Bottle Co.",
    "E J Painting & Fiberglass":                "E. J. Painting & Fiberglass",
    "Denver Metal Finishing":                   "Denver Metal Finishing",
    "Westlake Royal Roofing - Denver":          "Westlake Royal Roofing – Denver",
    "Suncor Energy Commerce City Refinery":     "Suncor Energy Commerce City Refinery",
    "Sinclair Denver Products Terminal":        "Sinclair Denver Products Terminal",
    "Kroger Mountain View Foods":               "Kroger Mountain View Foods",
}
for old, new in FIX.items():
    c = tbl.count(old)
    if c and old != new:
        tbl = tbl.replace(old, new)
        print("  %-42s -> %-42s x%d" % (old, new, c))
    elif not c:
        print("  NOT FOUND: %r" % old)

xml = xml[:ts] + tbl + xml[te:]
open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("document.xml rewritten")
