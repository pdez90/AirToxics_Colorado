"""Fold the two sections both numbered S5 into a single S5 Hotspots.

As submitted the SI had two Heading1 sections called S5:
    S5 Hotspot Sensitivity Analysis   (with S5.1 Methods, S5.2 Results)
    S5 Multipollutant Hotspot Characteristics

which makes "section S5" ambiguous and puts two different sections at the same
level in the outline. New structure:

    S5 Hotspots                                     (Heading1, new)
      S5.1 Hotspot Sensitivity Analysis             (Heading2, was Heading1 S5)
        S5.1.1 Methods                              (Heading3, was Heading2 S5.1)
        S5.1.2 Results                              (Heading3, was Heading2 S5.2)
      S5.2 Multipollutant Hotspot Characteristics   (Heading2, was Heading1 S5)

Also corrects S2's two subheadings, which were numbered 2.2.1 / 2.2.2 -- no S
prefix and a 2.2.x depth that does not exist in this document.

Figure and table numbers are untouched: Figures S5.1-S5.5 and Table S5.1 were
already a single continuous sequence across the two former sections, and they
stay continuous under the merged S5.

All new and changed heading text is red.
"""
import re

UP = "/tmp/sifig/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()
ps = re.findall(r'<w:p[ >].*?</w:p>', xml, re.S)

RED_RPR = '<w:rPr><w:color w:val="FF0000"/><w:rtl w:val="0"/></w:rPr>'


def retext(p, new):
    """Replace the paragraph's single text run with red `new`, keep bookmarks."""
    runs = re.findall(r'<w:r(?: [^>]*)?>.*?</w:r>', p, re.S)
    textruns = [r for r in runs if re.search(r'<w:t[^>]*>', r)]
    if len(textruns) != 1:
        raise SystemExit("expected 1 text run, got %d in: %r" % (len(textruns), p[:120]))
    newrun = '<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>' % (RED_RPR, new)
    return p.replace(textruns[0], newrun, 1)


def restyle(p, frm, to):
    if '<w:pStyle w:val="%s"/>' % frm not in p:
        raise SystemExit("style %s not found in: %r" % (frm, p[:120]))
    p = p.replace('<w:pStyle w:val="%s"/>' % frm, '<w:pStyle w:val="%s"/>' % to, 1)
    # drop Heading1's direct character formatting so the new level's style applies
    p = re.sub(r'(<w:pStyle w:val="%s"/>)<w:rPr>.*?</w:rPr>' % to, r'\1<w:rPr/>', p, count=1, flags=re.S)
    return p


EDITS = [
    # para, old style, new style, old text, new text
    (341, "Heading1", "Heading2", "S5 Hotspot Sensitivity Analysis",
                                  "S5.1 Hotspot Sensitivity Analysis"),
    (342, "Heading2", "Heading3", "S5.1 Methods",  "S5.1.1 Methods"),
    (350, "Heading2", "Heading3", "S5.2 Results",  "S5.1.2 Results"),
    (367, "Heading1", "Heading2", "S5 Multipollutant Hotspot Characteristics",
                                  "S5.2 Multipollutant Hotspot Characteristics"),
    (132, "Heading3", "Heading2", "2.2.1 Vocus Elf", "S2.1 Vocus Elf"),
    (134, "Heading3", "Heading2", "2.2.2 Vocus 2R",  "S2.2 Vocus 2R"),
]

for i, frm, to, oldt, newt in EDITS:
    p = ps[i]
    got = re.sub(r'<[^>]+>', '', "".join(re.findall(r'<w:t[^>]*>(.*?)</w:t>', p, re.S)))
    if got.strip() != oldt:
        raise SystemExit("para %d text is %r, expected %r" % (i, got, oldt))
    newp = retext(restyle(p, frm, to), newt)
    assert xml.count(p) == 1, "paragraph %d is not unique in the document" % i
    xml = xml.replace(p, newp, 1)
    print("  p%-4d %-9s -> %-9s  %-42s -> %s" % (i, frm, to, oldt, newt))

# ---- insert the new S5 Hotspots heading before the old S5 -------------
anchor = None
for p in re.findall(r'<w:p[ >].*?</w:p>', xml, re.S):
    if "S5.1 Hotspot Sensitivity Analysis" in p:
        anchor = p
        break
if anchor is None:
    raise SystemExit("could not find the demoted S5.1 heading to anchor to")

NEW_H1 = ('<w:p><w:pPr><w:pStyle w:val="Heading1"/><w:rPr/></w:pPr>'
          '<w:r>%s<w:t xml:space="preserve">S5 Hotspots</w:t></w:r></w:p>' % RED_RPR)
xml = xml.replace(anchor, NEW_H1 + anchor, 1)
print('  inserted Heading1 "S5 Hotspots" before S5.1')

open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("\ndocument.xml rewritten (%d bytes)" % len(xml.encode()))
