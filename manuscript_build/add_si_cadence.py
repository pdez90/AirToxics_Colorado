"""Add to SI S1.4 the two things it never described: the fourth HCN exclusion,
and how the delay correction and native-cadence averaging are implemented."""
import re, sys
sys.path.insert(0, "/tmp")

UP = "/tmp/sifig/unpacked"
RED = '<w:color w:val="FF0000"/>'
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

ANCHOR = "we restricted our analyses to HCN data collected on or after January 22, 2025."
i = xml.index(ANCHOR)
pend = xml.index("</w:p>", i) + len("</w:p>")

PPR = ('<w:pPr><w:spacing w:after="120" w:line="276" w:lineRule="auto"/>'
       '<w:jc w:val="both"/><w:rPr/></w:pPr>')

def para(text):
    out = []
    for part in re.split(r"(H2S|H2O)", text):
        if part == "H2S":
            out.append('<w:r><w:rPr>%s<w:rtl w:val="0"/></w:rPr><w:t xml:space="preserve">H</w:t></w:r>' % RED)
            out.append('<w:r><w:rPr>%s<w:vertAlign w:val="subscript"/><w:rtl w:val="0"/></w:rPr><w:t xml:space="preserve">2</w:t></w:r>' % RED)
            out.append('<w:r><w:rPr>%s<w:rtl w:val="0"/></w:rPr><w:t xml:space="preserve">S</w:t></w:r>' % RED)
        elif part:
            t = part.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
            out.append('<w:r><w:rPr>%s<w:rtl w:val="0"/></w:rPr><w:t xml:space="preserve">%s</w:t></w:r>' % (RED, t))
    return "<w:p>%s%s</w:p>" % (PPR, "".join(out))

NEW = [
 "CDPHE further reports that an inaccurate sensitivity calibration performed in "
 "the week of 27 May 2025 left the HCN concentrations for 28, 29 and 30 May 2025 "
 "resting on averaged calibrations rather than a valid one. Those three days "
 "were removed; two of them carry measurements, and 29,085 HCN values are "
 "dropped.",

 "Two processing steps precede all of the analyses in this work. First, each "
 "pollutant is shifted back by its measured instrument sampling delay, so that a "
 "concentration is attributed to the location at which the air was drawn in "
 "rather than the location at which the instrument reported it. The delays are "
 "asset-specific and were measured rather than assumed: for the CAT mobile "
 "laboratory 4 s for the aromatics, 6 s for HCN and 21 s for H2S, and for the "
 "EMU laboratory 5 s, 3 s and 17 s respectively.",

 "Second, the two species delivered on a one-second grid but acquired more "
 "slowly are averaged to their native acquisition cadence: H2S over 5 s, the "
 "Picarro G2204 acquisition interval, and HCN over 2 s, the Vocus acquisition "
 "interval. The averaging block is anchored on the instrument's own delivery "
 "clock, by adding the delay back before forming the block index, because the "
 "delays are not multiples of the cadence and differ between the two "
 "laboratories; anchoring on the shifted timestamp would blend parts of two "
 "consecutive delivered readings with a laboratory-specific weight and attenuate "
 "peak amplitudes differently in the two vehicles. The block mean is written "
 "back only to seconds that already held a value, so the procedure de-noises the "
 "delivered record to its true resolution without gap-filling: coverage and "
 "campaign medians are unchanged. The aromatics are delivered at one second and "
 "are not averaged. The plume-inversion analysis is exempt and uses the "
 "un-averaged delivered H2S signal, retained alongside the averaged column, "
 "because averaging flattens the rise and fall that plume identification depends "
 "on.",
]

xml = xml[:pend] + "".join(para(t) for t in NEW) + xml[pend:]
open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("added %d paragraphs to S1.4" % len(NEW))
print("document.xml now %d bytes" % len(xml.encode()))
