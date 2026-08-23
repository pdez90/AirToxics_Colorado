"""Insert new Section S7 (screening cumulative noncancer hazard assessment)
into the SI, all in red, immediately before the References heading, and append
references 29-34 to the reference list.

Every number is the one emitted by R_scripts/74_health_hazard_screening.R
(census-block basis, ppb->ug/m3 at the 830 hPa site pressure, consistent with
the Section 2.4 IUR conversion), to be confirmed by the terminal run of 74;
the cell-based sensitivity values in S7.3 are from the 2026-08-22 pipeline run
of 73_cumulative_risk.R (TABLE_cumulative_HI_completeset.csv).
Formatting mirrors the document's own conventions: Heading1/2 styles, plain
body paragraphs, Table1-style bordered tables (9360 dxa), Nature-style
numbered references (spacing line=480, ind left=384, "N." + tab).
"""
import re

P = "unpacked/word/document.xml"
d = open(P, encoding="utf-8").read()

RED = '<w:color w:val="FF0000"/>'

def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

def run(text, bold=False, ital=False, sup=False, sub=False, red=True):
    # CT_RPr sequence: b,bCs,i,iCs ... color ... vertAlign, rtl
    rpr = ""
    if bold: rpr += '<w:b w:val="1"/><w:bCs w:val="1"/>'
    if ital: rpr += '<w:i w:val="1"/><w:iCs w:val="1"/>'
    if red:  rpr += RED
    if sup:  rpr += '<w:vertAlign w:val="superscript"/>'
    if sub:  rpr += '<w:vertAlign w:val="subscript"/>'
    rpr += '<w:rtl w:val="0"/>'
    return '<w:r><w:rPr>%s</w:rPr><w:t xml:space="preserve">%s</w:t></w:r>' % (rpr, esc(text))

def para(runs, style=None, center=False, ppr_extra=""):
    ppr = "<w:pPr>"
    if style: ppr += '<w:pStyle w:val="%s"/>' % style
    if center: ppr += '<w:jc w:val="center"/>'
    ppr += ppr_extra + "<w:rPr/></w:pPr>"
    return "<w:p>%s%s</w:p>" % (ppr, "".join(runs))

def h1(text): return para([run(text)], style="Heading1")
def h2(text): return para([run(text)], style="Heading2")

# H2S with subscript 2
def h2s(bold=False):
    return run("H", bold=bold) + run("2", bold=bold, sub=True) + run("S", bold=bold)

def cite(nums):     # superscript red citation like 29,31
    return run(nums, sup=True)

# ---------------- tables ----------------
# CT_TcPr sequence: tcW, gridSpan, ... , shd, noWrap, tcMar, textDirection,
# tcFitText, vAlign, hideMark
def cell(runs_list, w, bold=False, fill=None):
    shd = '<w:shd w:fill="%s" w:val="clear"/>' % (fill or "auto")
    tcpr = ('<w:tcPr><w:tcW w:w="%d" w:type="dxa"/>%s'
            '<w:tcMar><w:top w:w="60" w:type="dxa"/>'
            '<w:left w:w="80" w:type="dxa"/><w:bottom w:w="60" w:type="dxa"/>'
            '<w:right w:w="80" w:type="dxa"/></w:tcMar>'
            '<w:vAlign w:val="center"/></w:tcPr>') % (w, shd)
    p = ('<w:p><w:pPr><w:widowControl w:val="0"/>'
         '<w:spacing w:line="240" w:lineRule="auto"/><w:jc w:val="center"/>'
         '<w:rPr/></w:pPr>%s</w:p>') % "".join(runs_list)
    return "<w:tc>%s%s</w:tc>" % (tcpr, p)

def cell_span(runs_list, w, span, fill=None):
    shd = '<w:shd w:fill="%s" w:val="clear"/>' % (fill or "auto")
    tcpr = ('<w:tcPr><w:tcW w:w="%d" w:type="dxa"/><w:gridSpan w:val="%d"/>%s'
            '<w:tcMar><w:top w:w="60" w:type="dxa"/>'
            '<w:left w:w="80" w:type="dxa"/><w:bottom w:w="60" w:type="dxa"/>'
            '<w:right w:w="80" w:type="dxa"/></w:tcMar>'
            '<w:vAlign w:val="center"/></w:tcPr>') % (w, span, shd)
    p = ('<w:p><w:pPr><w:widowControl w:val="0"/>'
         '<w:spacing w:line="240" w:lineRule="auto"/>'
         '<w:rPr/></w:pPr>%s</w:p>') % "".join(runs_list)
    return "<w:tc>%s%s</w:tc>" % (tcpr, p)

def row(cells):
    return ('<w:tr><w:trPr><w:cantSplit w:val="0"/><w:tblHeader w:val="0"/></w:trPr>%s</w:tr>'
            % "".join(cells))

def table(widths, rows):
    grid = "".join('<w:gridCol w:w="%d"/>' % w for w in widths)
    return ('<w:tbl><w:tblPr><w:tblStyle w:val="Table1"/><w:tblW w:w="9360.0" w:type="dxa"/>'
            '<w:jc w:val="left"/><w:tblBorders>'
            '<w:top w:color="000000" w:space="0" w:sz="8" w:val="single"/>'
            '<w:left w:color="000000" w:space="0" w:sz="8" w:val="single"/>'
            '<w:bottom w:color="000000" w:space="0" w:sz="8" w:val="single"/>'
            '<w:right w:color="000000" w:space="0" w:sz="8" w:val="single"/>'
            '<w:insideH w:color="000000" w:space="0" w:sz="8" w:val="single"/>'
            '<w:insideV w:color="000000" w:space="0" w:sz="8" w:val="single"/>'
            '</w:tblBorders><w:tblLayout w:type="fixed"/><w:tblLook w:val="0600"/></w:tblPr>'
            '<w:tblGrid>%s</w:tblGrid>%s</w:tbl>') % (grid, "".join(rows))

UGM3 = lambda bold=False: [run("(µg m", bold=bold), run("−3", bold=bold, sup=True), run(")", bold=bold)]

# ================= build S7 =================
S7 = []

S7.append(h1("S7 Screening-Level Cumulative Noncancer Hazard Assessment"))

S7.append(para([
  run("Section 3.3 of the main text quantifies excess lifetime "),
  run("cancer", ital=True),
  run(" risk from benzene. Here we complement that analysis with a screening-level, cumulative "),
  run("noncancer", ital=True),
  run(" hazard assessment across all six measured pollutants. We follow the hazard-quotient / hazard-index framework recently applied to fenceline-community mobile-monitoring data by Chiger et al."),
  cite("29"),
  run(", which operationalizes U.S. EPA guidance for the cumulative risk assessment of chemical mixtures. This mirrors, on the noncancer side, the measurement-based risk translations of Robinson et al."),
  cite("31,32"),
  run(", and responds to the observation from community-based work in fenceline neighborhoods that a health assessment restricted to the single most-sensitive endpoint of each chemical can understate the cumulative burden residents actually experience."),
  cite("30"),
]))

S7.append(h2("S7.1 Methods"))
S7.append(para([
  run("For each pollutant "), run("i", ital=True),
  run(" we compute a hazard quotient, the ratio of an exposure concentration to the chronic inhalation reference concentration (RfC), and sum the hazard quotients of the pollutants acting on a common target organ system into a hazard index:"),
  cite("29"),
]))
S7.append(para([
  run("HQ", ital=True), run("i", ital=True, sub=True), run(" = EC"),
  run("i", ital=True, sub=True), run(" / RfC"), run("i", ital=True, sub=True),
  run("          (Eq. S7.1)")], center=True))
S7.append(para([
  run("HI", ital=True), run("organ", ital=True, sub=True), run(" = Σ"),
  run("i", ital=True, sub=True), run(" HQ"), run("i", ital=True, sub=True),
  run("          (Eq. S7.2)")], center=True))
S7.append(para([
  run("where EC"), run("i", ital=True, sub=True),
  run(" is the exposure concentration and RfC"), run("i", ital=True, sub=True),
  run(" the U.S. EPA Integrated Risk Information System (IRIS) chronic inhalation reference concentration."),
  cite("33"),
  run(" A hazard quotient or index at or above 1 flags an exposure above the level judged, with uncertainty spanning perhaps an order of magnitude, to be without appreciable risk of that effect over a lifetime. Each pollutant is assigned to the target organ system of its IRIS critical effect (Table S7.1); mixing ratios (ppb) are converted to mass concentrations (µg m"),
  run("−3", sup=True),
  run(") at 25 °C and the 830 hPa site pressure (molar volume 29.87 L mol"),
  run("−1", sup=True),
  run("), the same convention used to express the benzene inhalation unit risks in Section 2.4."),
]))
S7.append(para([
  run("We characterize exposure two ways from the block-resolved concentrations used in Section 3.3: a "),
  run("population-weighted mean", bold=True),
  run(" of the block mean-of-daily-mean concentration, representing a community-average long-term exposure, and the single "),
  run("most-exposed census block", bold=True),
  run(" as a worst-case. Consistent with every other pipeline stage, negative and below-MDL values are retained in the block means (Section S1.4); population weighting and time-averaging keep the community metric unbiased. We use the block concentrations without the stationary-site temporal-bias scaling of Section 3.3, so that all six pollutants are treated identically — the scaling factors were derived only for the three aromatics. Applying the aromatic scaling raises the neurological and hematological hazard indices by less than 0.01 and leaves the endocrine and respiratory indices, which are set by unscaled species ("),
] + [run("H"), run("2", sub=True), run("S and HCN), unchanged.")]))
S7.append(para([
  run("As a companion "), run("acute", ital=True),
  run(" screen, we compare the campaign 99th-percentile and maximum short-term concentrations (Table S3.1) with the California OEHHA 1-hour acute Reference Exposure Levels (RELs)."),
  cite("34"),
  run(" Because our measurements resolve sub-minute peaks while the REL averaging time is one hour, these acute hazard quotients are deliberately conservative upper bounds rather than estimates of a realized one-hour exposure."),
]))
S7.append(para([
  run("We follow the most-sensitive-organ (“traditional”) approach of Chiger et al."),
  cite("29"),
  run(" Those authors also pilot an expanded multi-effects toxicity database that assigns each chemical to "),
  run("every", ital=True),
  run(" target organ system with an available toxicity value, and show that doing so can raise hazard indices above 1 where the traditional approach finds no concern. With only six pollutants, each carrying a single IRIS RfC, we retain the traditional assignment and discuss this expansion as a limitation (Section S7.3) rather than deriving new toxicity values."),
]))

# ---- Table S7.1 ----
S7.append(para([
  run("Table S7.1: ", bold=True),
  run("Chronic noncancer hazard quotients and organ-system hazard indices for the six measured pollutants. Reference concentrations are EPA IRIS chronic inhalation RfCs;"),
  cite("33"),
  run(" the population-weighted-mean and most-exposed-block columns are the two exposure metrics defined in Section S7.1. Hazard indices at or above 1 are in bold."),
]))
W1 = [1900, 1400, 1150, 1150, 1150, 1300, 1310]
HDR = "D9D9D9"
rows1 = [row([
  cell([run("Pollutant", bold=True)], W1[0], fill=HDR),
  cell([run("Target organ system", bold=True)], W1[1], fill=HDR),
  cell([run("IRIS RfC ", bold=True)] + UGM3(True), W1[2], fill=HDR),
  cell([run("Pop-wt mean ", bold=True)] + UGM3(True), W1[3], fill=HDR),
  cell([run("HQ, pop-wt mean", bold=True)], W1[4], fill=HDR),
  cell([run("Most-exposed block ", bold=True)] + UGM3(True), W1[5], fill=HDR),
  cell([run("HQ, most-exposed block", bold=True)], W1[6], fill=HDR),
])]
for name, organ, rfc, pw, hqpw, mx, hqmx in [
    ([run("Benzene")], "Hematological", "30", "0.461", "0.015", "7.11", "0.237"),
    ([run("Toluene")], "Neurological", "5000", "1.42", "0.0003", "50.7", "0.010"),
    ([run("Xylenes")], "Neurological", "100", "1.21", "0.012", "32.9", "0.329"),
    ([run("1,2,4-Trimethylbenzene")], "Neurological", "60", "1.15", "0.019", "12.9", "0.216"),
    ([run("H"), run("2", sub=True), run("S")], "Respiratory", "2", "0.748", "0.374", "9.95", "4.97"),
    ([run("HCN")], "Endocrine (thyroid)", "0.8", "1.28", "1.60", "6.97", "8.71"),
]:
    rows1.append(row([
      cell(name, W1[0]), cell([run(organ)], W1[1]), cell([run(rfc)], W1[2]),
      cell([run(pw)], W1[3]), cell([run(hqpw)], W1[4]),
      cell([run(mx)], W1[5]), cell([run(hqmx)], W1[6])]))
rows1.append(row([
  cell_span([run("Hazard index by organ system", bold=True)], sum(W1[:4]), 4, fill=HDR),
  cell([run("HI, pop-wt mean", bold=True)], W1[4], fill=HDR),
  cell([run("", red=True)], W1[5], fill=HDR),
  cell([run("HI, most-exposed block", bold=True)], W1[6], fill=HDR)]))
for lab, pw, mx, bold_pw, bold_mx in [
    ([run("Endocrine (HCN)")], "1.60", "8.71", True, True),
    ([run("Respiratory ("), run("H"), run("2", sub=True), run("S)")], "0.374", "4.97", False, True),
    ([run("Neurological (toluene + xylenes + 1,2,4-TMB)")], "0.031", "0.555", False, False),
    ([run("Hematological (benzene)")], "0.015", "0.237", False, False),
]:
    rows1.append(row([
      cell_span(lab, sum(W1[:4]), 4),
      cell([run(pw, bold=bold_pw)], W1[4]),
      cell([run("", red=True)], W1[5]),
      cell([run(mx, bold=bold_mx)], W1[6])]))
S7.append(table(W1, rows1))

S7.append(h2("S7.2 Results"))
S7.append(para([
  run("At the population-weighted-mean exposure, four of the six pollutants give hazard quotients well below 1. The exception is hydrogen cyanide: its low reference concentration (0.8 µg m"),
  run("−3", sup=True),
  run(") yields an endocrine (thyroid) hazard index of "),
  run("1.60", bold=True),
  run(" at the community average and "),
  run("8.71", bold=True),
  run(" in the most-exposed block. Hydrogen sulfide gives a respiratory hazard index of 0.37 at the community average but "),
  run("4.97", bold=True),
  run(" in the most-exposed block, so it too exceeds 1 for the most-exposed residents. The neurological hazard index (toluene, xylenes and 1,2,4-trimethylbenzene combined) reaches only 0.031 at the community average and 0.56 at the most-exposed block, and the hematological index for benzene reaches 0.015 and 0.24 respectively; neither approaches 1 anywhere in the domain. This pattern — endocrine and respiratory effects exceeding 1 while neurological, hematological and other systems remain below — is consistent with the fenceline cumulative-risk results of Chiger et al.,"),
  cite("29"),
  run(" in which endocrine, renal, respiratory and neurological indices exceeded 1 once effects beyond the most-sensitive endpoint were considered."),
]))
S7.append(para([
  run("The acute screen (Table S7.2) tells a complementary story. At the 99th-percentile short-term concentration every pollutant sits below its OEHHA 1-hour acute REL (largest HQ 0.17, benzene). Only at the single highest instantaneous peak of the entire campaign do benzene (HQ ≈ 55), "),
  run("H"), run("2", sub=True),
  run("S (HQ ≈ 9.4) and toluene (HQ ≈ 1.8) exceed the 1-hour REL; the trimethylbenzene peak reaches 40% of its REL (HQ ≈ 0.40). Because those peaks are sub-minute excursions compared against a one-hour guideline, they bound rather than estimate an acute exposure; they nonetheless mark benzene and "),
  run("H"), run("2", sub=True),
  run("S — and to a lesser degree toluene — as the species whose transient plumes most warrant follow-up with time-resolved acute metrics."),
]))

# ---- Table S7.2 ----
S7.append(para([
  run("Table S7.2: ", bold=True),
  run("Acute screen: campaign 99th-percentile and maximum short-term concentrations (Table S3.1) versus OEHHA 1-hour acute RELs"),
  cite("34"),
  run(" (current REL summary, accessed August 2026; the toluene REL reflects OEHHA’s 2020 revision and the trimethylbenzene REL the 2023 adoption). Ratios at the maximum are conservative upper bounds (sub-minute peak vs 1-hour averaging time). Hazard quotients at or above 1 are in bold."),
]))
W2 = [2100, 1550, 1450, 1300, 1550, 1410]
rows2 = [row([
  cell([run("Pollutant", bold=True)], W2[0], fill=HDR),
  cell([run("OEHHA acute REL ", bold=True)] + UGM3(True), W2[1], fill=HDR),
  cell([run("p99 conc. ", bold=True)] + UGM3(True), W2[2], fill=HDR),
  cell([run("HQ at p99", bold=True)], W2[3], fill=HDR),
  cell([run("Max conc. ", bold=True)] + UGM3(True), W2[4], fill=HDR),
  cell([run("HQ at max", bold=True)], W2[5], fill=HDR),
])]
for name, rel, p99, h99, mx, hmx, flag in [
    ([run("Benzene")], "27", "4.71", "0.174", "1485.8", "55.0", True),
    ([run("Toluene")], "5000", "13.30", "0.00266", "8831.9", "1.77", True),
    ([run("Xylenes")], "22000", "11.34", "0.00052", "4469.5", "0.203", False),
    ([run("1,2,4-Trimethylbenzene")], "2400", "10.42", "0.00434", "969.8", "0.404", False),
    ([run("H"), run("2", sub=True), run("S")], "42", "5.48", "0.130", "394.4", "9.39", True),
    ([run("HCN")], "340", "9.96", "0.0293", "65.2", "0.192", False),
]:
    rows2.append(row([
      cell(name, W2[0]), cell([run(rel)], W2[1]), cell([run(p99)], W2[2]),
      cell([run(h99)], W2[3]), cell([run(mx)], W2[4]),
      cell([run(hmx, bold=flag)], W2[5])]))
S7.append(table(W2, rows2))

S7.append(h2("S7.3 Interpretation and Limitations"))
S7.append(para([
  run("This is a "), run("screening", ital=True),
  run(" assessment, not a formal exposure or risk assessment, and several limitations bound its interpretation. First, the mobile campaign is a repeated but finite set of drive-days weighted toward winter conditions; translating measured concentrations into the lifetime average that a reference concentration presumes carries real uncertainty, a caveat shared by every measurement-based HAP risk study of this kind."),
  cite("29,31,32"),
  run(" Second, the endocrine exceedance rests entirely on hydrogen cyanide, whose reference concentration is the lowest of the six and whose record is the thinnest: HCN passes QA/QC only from 22 January 2025 onward (39 measurement days, three May calibration days excluded) and covers ~90,000 of the ~127,000 residents. The thyroid endpoint and the magnitude of the exceedance make HCN a clear priority for targeted follow-up, but the single-species, short-record basis means the endocrine index should be read as a flag, not a settled estimate. Third, the respiratory exceedance for "),
  run("H"), run("2", sub=True),
  run("S is confined to the most-exposed blocks along the corridor near the wastewater-treatment facility identified in Section 3.4, rather than being community-wide. Fourth, single-measurement precision is limited exactly where the hazard signal sits: the audited method detection limits for "),
  run("H"), run("2", sub=True),
  run("S (2–6 ppb, depending on laboratory and period) and HCN (0.18–13 ppb) exceed the mixing ratios equivalent to their RfCs (1.75 and 0.88 ppb at site pressure), so individual readings near the reference level are below detection; the hazard metrics rest on averages over many readings, which remain unbiased because negative and below-MDL values are retained (Section S1.4)."),
]))
S7.append(para([
  run("Finally, the hazard-index framework assumes dose-additivity within an organ system and, in the traditional form used here, counts only each chemical’s most-sensitive effect. Both choices are likely to "),
  run("under", ital=True),
  run("state cumulative hazard: benzene also acts on the immune system and hydrogen cyanide has a secondary central-nervous-system effect, and Chiger et al."),
  cite("29"),
  run(" show that incorporating additional target organ systems through a multi-effects toxicity database raises several hazard indices above 1 where a most-sensitive-organ analysis finds none. The choice of reference values matters in the same direction: California OEHHA’s chronic RELs"),
  cite("34"),
  run(" for benzene (3 µg m"),
  run("−3", sup=True),
  run(") and the trimethylbenzenes (4 µg m"),
  run("−3", sup=True),
  run(") sit 10- and 15-fold below the corresponding IRIS RfCs, and re-anchoring to them would raise the most-exposed-block hazard quotients to 2.37 (benzene, hematological) and 3.24 (1,2,4-trimethylbenzene, neurological) — above 1 — while leaving the community-average picture qualitatively unchanged. The results are also robust to the exposure construction: an independent analysis on the 500 m grid used for the concentration maps (background-corrected concentrations, cells with at least 10 visit-days) reproduces the same ordering, with the endocrine index above 1 in 95% of cells at the mean-based exposure metric (maximum 2.89) and the respiratory index above 1 only in the most-exposed cells (maximum 6.19). A full multi-effects treatment for this pollutant suite, together with time-resolved acute metrics, is a natural next step."),
]))

S7_XML = "".join(S7)

# ---- insert before the References Heading1 paragraph ----
i = d.find(">References<")
assert i > 0
ps = d.rfind("<w:p ", 0, i)
assert ps > 0
d = d[:ps] + S7_XML + d[ps:]
print("S7 inserted before References heading (%d chars)" % len(S7_XML))

# ---- append references 29-34 (Nature style: italic journal, bold volume) ----
def refpara(num, runs_list):
    ppr = ('<w:pPr><w:spacing w:after="0" w:line="480" w:lineRule="auto"/>'
           '<w:ind w:left="384.00000000000006"/><w:rPr/></w:pPr>')
    r1 = run("%d." % num)
    tab = '<w:r><w:rPr>%s<w:rtl w:val="0"/></w:rPr><w:tab/></w:r>' % RED
    return "<w:p>%s%s%s%s</w:p>" % (ppr, r1, tab, "".join(runs_list))

REFS = [
 (29, [run("Chiger, A. A. "), run("et al.", ital=True),
       run(" Improving methodologies for cumulative risk assessment: a case study of noncarcinogenic health risks from volatile organic compounds in fenceline communities in Southeastern Pennsylvania. "),
       run("Environ. Health Perspect.", ital=True), run(" "), run("133", bold=True),
       run(", 057004 (2025). doi:10.1289/EHP14696")]),
 (30, [run("Chiger, A. A. "), run("et al.", ital=True),
       run(" Influences of chemical and nonchemical stressors on health and quality of life in fenceline communities: a community-based participatory research survey in Southeastern Pennsylvania. "),
       run("Environ. Justice", ital=True),
       run(" (2025). doi:10.1089/env.2024.0078")]),
 (31, [run("Robinson, E. S. "), run("et al.", ital=True),
       run(" Total cancer risk estimates from measured concentrations of volatile organic compounds in industrialized southeastern Louisiana. "),
       run("Proc. Natl. Acad. Sci. U.S.A.", ital=True), run(" "), run("122", bold=True),
       run(", e2504770122 (2025). doi:10.1073/pnas.2504770122")]),
 (32, [run("Robinson, E. S. "), run("et al.", ital=True),
       run(" Ethylene oxide in southeastern Louisiana's petrochemical corridor: high spatial resolution mobile monitoring during HAP-MAP. "),
       run("Environ. Sci. Technol.", ital=True), run(" "), run("58", bold=True),
       run(", 11084-11095 (2024). doi:10.1021/acs.est.3c10579")]),
 (33, [run("U.S. EPA. Integrated Risk Information System (IRIS): chronic inhalation reference concentrations for benzene (2003), toluene (2005), xylenes (2003), 1,2,4-trimethylbenzene (2016), hydrogen sulfide (2003), and hydrogen cyanide and cyanide salts (2010). https://iris.epa.gov (accessed August 2026).")]),
 (34, [run("OEHHA (California Office of Environmental Health Hazard Assessment). Acute, 8-hour and chronic Reference Exposure Level (REL) summary. https://oehha.ca.gov/air/general-info/oehha-acute-8-hour-and-chronic-reference-exposure-level-rel-summary (accessed August 2026).")]),
]
REFS_XML = "".join(refpara(n, rl) for n, rl in REFS)

j = d.find("28.</w:t>")
assert j > 0, "reference 28 not found"
pe = d.find("</w:p>", j) + len("</w:p>")
d = d[:pe] + REFS_XML + d[pe:]
print("references 29-34 appended after reference 28")

open(P, "w", encoding="utf-8").write(d)
print("document.xml rewritten (%d bytes)" % len(d))
