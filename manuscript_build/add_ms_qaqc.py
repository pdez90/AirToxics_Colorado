"""Add to Methods 2.1.1 a paragraph stating what QA/QC rule was applied, which
windows were excluded and on whose authority, and how the delay correction and
native-cadence averaging work. None of this was in the main text."""
import re

UP = "/tmp/msfix/unpacked"
RED = '<w:color w:val="FF0000"/>'
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

ANCHOR = "More information about the instruments and QA/QC processes is provided in "
i = xml.index(ANCHOR)
pend = xml.index("</w:p>", i) + len("</w:p>")

PPR = ('<w:pPr><w:spacing w:after="120" w:line="276" w:lineRule="auto"/>'
       '<w:jc w:val="both"/><w:rPr/></w:pPr>')

def para(text):
    out = []
    for part in re.split(r"(H2S)", text):
        if part == "H2S":
            out += ['<w:r><w:rPr>%s<w:rtl w:val="0"/></w:rPr><w:t xml:space="preserve">H</w:t></w:r>' % RED,
                    '<w:r><w:rPr>%s<w:vertAlign w:val="subscript"/><w:rtl w:val="0"/></w:rPr><w:t xml:space="preserve">2</w:t></w:r>' % RED,
                    '<w:r><w:rPr>%s<w:rtl w:val="0"/></w:rPr><w:t xml:space="preserve">S</w:t></w:r>' % RED]
        elif part:
            t = part.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
            out.append('<w:r><w:rPr>%s<w:rtl w:val="0"/></w:rPr><w:t xml:space="preserve">%s</w:t></w:r>' % (RED, t))
    return "<w:p>%s%s</w:p>" % (PPR, "".join(out))

TEXT = (
 "One quality-control rule was applied identically to all six pollutants: a "
 "measurement is voided only when its CDPHE flag string carries a null data "
 "qualifier. Values below the method detection limit and negative values are "
 "retained, and we substitute neither zero nor half the detection limit for "
 "them; we verified that the delivered one-second files contain no such "
 "substitution either. Four periods were excluded, each on CDPHE's own advice "
 "as recorded in the quarterly read-me documents that accompany the data: 16 "
 "April to 20 September 2023 for benzene, toluene, trimethylbenzene, xylene and "
 "HCN, after CDPHE discovered inlet line contamination that it reports is not "
 "corrected for in the delivered data; 13 and 14 August 2024 for the four "
 "aromatics, when the Vocus Eiger's automatic baseline correction was not "
 "operational; all HCN on or before 22 January 2025, which CDPHE background-"
 "corrected rather than reporting as absolute; and HCN on 28 to 30 May 2025, "
 "affected by an inaccurate sensitivity calibration. H2S is measured by a "
 "separate Picarro analyser, is not among the compounds CDPHE identifies as "
 "contaminated in 2023, and shows no elevation across that window in our own "
 "record, so it was retained throughout it. Before any analysis, each pollutant "
 "was shifted back by its measured, laboratory-specific instrument sampling "
 "delay, and the two species delivered faster than they are acquired were "
 "averaged to their native acquisition cadence, H2S over 5 s and HCN over 2 s, "
 "with each block mean written back only to seconds that already carried a "
 "value so that no gap is filled. Section S1.4 gives the detail, including the "
 "one place where CDPHE's read-me documents and the delivered files disagree.")

xml = xml[:pend] + para(TEXT) + xml[pend:]
open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("Methods 2.1.1: QA/QC and processing paragraph added")
print("document.xml now %d bytes" % len(xml.encode()))
