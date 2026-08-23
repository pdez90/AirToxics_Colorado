# -*- coding: utf-8 -*-
"""Restore the SI sections lost in the 2026-08-21 rebuild, from the 2026-08-20
SI_Part1-3 drafts, with every number updated to the current pipeline run, and
add the methane section as S8 (the cumulative-hazard section remains S7).
All restored text is red. Figures are embedded from /tmp/sifig/figs/<name>.png
(current-run files staged from FinalFig); the script fails loudly if a listed
figure file is missing.

Current-run sources for every number (mined 2026-08-23):
  FIG_R log: cat-emu (29/29/29/29/27/12 cell-days; ratios 1.00/0.99/0.70/0.75;
    Spearman 0.167-0.446; med abs diff H2S 1.0, HCN 1.5), 45 substitution
    (95.2/39.0/76.1/55.5/97.5/50.5 below-MDL; medians 0/0.10/0.60/1.20;
    p95 full 3.2; blocks 1665; r 0.786/0.351/0.013; ratios 0.66/3.31/6.58),
    50 cells below MDL (99.3/20.6/71.9/47.3/100/100), 56 smoke (157/38/5/3;
    benzene ratio 1.00 p .013; toluene 0.78 p .012; xylene 0.66 p .006;
    H2S 1.62 p .27), 62 seasonal (48/63/52/40; values per table)
  FIG_P log: 47 grid (0-66 groups; recovery 0-94%; baseline 17 @94%),
    52 split (odd 20/even 16; 0.40/0.56; base 0.82/0.53; cal 14/17;
    0.43/0.35; base 0.53/0.65), 60 sufficiency (map cor .58-.99;
    recovery 12->94%)
  FIG_V log: 46 grid (D3.2: 8.8/30/96 t/yr at 0.5/1/2 km; B3.2 2km 716;
    D 0.5km 5.5-13.7 across 2-5 m/s), per-plume qmin (408/675/358/701 MDL5;
    326-490/540-810/287-430/560-841 MDL4-6; SNR 1.2/2.8/1.0/1.0)
  inversion CSV: plumes 2/10/13/32 -> 706/1964/471/1003 tpy; dists
    4.11/1.95/3.55/4.30 km; dH2S 6/14/5.2/5; stab C/B/D/C
  attribution CSV: dCH4 0.094/none/0.02/0.038; all WWTP-consistent
  methane logs: 1,837,632 obs/193 days; p99 2.575 (p95 2.283); 18,378 events
    on 160 days; 66 clusters; persistent >=16 days; cl1 39.8233/-104.9741
    11,843 ev/66 d/max 19.3; cl2 39.7818/-104.9860 2,841 ev/57 d/max 10.9;
    wind join 90.3% after hourly fallback; sourceprob max 39.872/-104.876
  methane_at_toxics CSV: 0.2-36.8%; G9 36.8%/11 d; 10 of 17 <5%
  EJ (container GEOID join): median pctl 86.4 / DI 81.3% domain; >2x 84.8 /
    79.6% vs 86.4 / 81.4%; p 0.96  [her 59 rerun confirms]
  patched 54 expected: HQ 0.040/0.001/0.025/0.027/1.398/1.697;
    rfc_ppb 11.47/1620.7/14.91/28.13/1.75/0.88; max cell 24h 0.46/1.044/
    0.38/0.757/2.45/1.5  [her S rerun confirms]
  her X run: 48 scaling factors/ratios, 49 day-night ratios, 58 bootstrap,
    61 CPF  -> values filled from her console paste before this ran
"""
import re, os, struct

P = "unpacked/word/document.xml"
RELS = "unpacked/word/_rels/document.xml.rels"
d = open(P, encoding="utf-8").read()
rels = open(RELS, encoding="utf-8").read()

RED = '<w:color w:val="FF0000"/>'

def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

def run(text, bold=False, ital=False, sup=False, sub=False):
    rpr = ""
    if bold: rpr += '<w:b w:val="1"/><w:bCs w:val="1"/>'
    if ital: rpr += '<w:i w:val="1"/><w:iCs w:val="1"/>'
    rpr += RED
    if sup: rpr += '<w:vertAlign w:val="superscript"/>'
    if sub: rpr += '<w:vertAlign w:val="subscript"/>'
    rpr += '<w:rtl w:val="0"/>'
    return '<w:r><w:rPr>%s</w:rPr><w:t xml:space="preserve">%s</w:t></w:r>' % (rpr, esc(text))

def para(runs, style=None, center=False):
    ppr = "<w:pPr>"
    if style: ppr += '<w:pStyle w:val="%s"/>' % style
    if center: ppr += '<w:jc w:val="center"/>'
    ppr += "<w:rPr/></w:pPr>"
    return "<w:p>%s%s</w:p>" % (ppr, "".join(runs))

def h2s(bold=False, ital=False):
    return (run("H", bold=bold, ital=ital) + run("2", bold=bold, ital=ital, sub=True)
            + run("S", bold=bold, ital=ital))

# ------- figures -------
def png_size(path):
    with open(path, "rb") as f:
        f.read(16)
        w, h = struct.unpack(">II", f.read(8))
    return w, h

_next_rid = max(int(m) for m in re.findall(r'Id="rId(\d+)"', rels)) + 1
_media_added = []

def figure(fname, width_in=6.0):
    """Embed figs/<fname> as an inline image paragraph; returns XML."""
    global rels, _next_rid
    src = os.path.join("figs", fname)
    assert os.path.exists(src), "figure file missing: " + src
    w, h = png_size(src)
    cx = int(width_in * 914400)
    cy = int(cx * h / w)
    n = len(_media_added) + 1
    media_name = "imageR%02d.png" % n
    rid = "rId%d" % _next_rid; _next_rid += 1
    _media_added.append((src, media_name))
    rels = rels.replace("</Relationships>",
        '<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/%s"/></Relationships>' % (rid, media_name))
    draw = ('<w:drawing><wp:inline distB="114300" distT="114300" distL="114300" distR="114300">'
            '<wp:extent cx="%d" cy="%d"/><wp:effectExtent b="0" l="0" r="0" t="0"/>'
            '<wp:docPr id="9%02d" name="%s"/><a:graphic>'
            '<a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">'
            '<pic:pic><pic:nvPicPr><pic:cNvPr id="0" name="%s"/>'
            '<pic:cNvPicPr preferRelativeResize="0"/></pic:nvPicPr>'
            '<pic:blipFill><a:blip r:embed="%s"/><a:srcRect b="0" l="0" r="0" t="0"/>'
            '<a:stretch><a:fillRect/></a:stretch></pic:blipFill>'
            '<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="%d" cy="%d"/></a:xfrm>'
            '<a:prstGeom prst="rect"/><a:ln/></pic:spPr></pic:pic>'
            '</a:graphicData></a:graphic></wp:inline></w:drawing>'
            % (cx, cy, n, media_name, media_name, rid, cx, cy))
    return '<w:p><w:pPr><w:jc w:val="center"/><w:rPr/></w:pPr><w:r><w:rPr><w:rtl w:val="0"/></w:rPr>%s</w:r></w:p>' % draw

# ------- tables (CT_TcPr order as fixed for S7) -------
HDR = "D9D9D9"
def cell(runs_list, w, fill=None, span=1):
    shd = '<w:shd w:fill="%s" w:val="clear"/>' % (fill or "auto")
    gs = ('<w:gridSpan w:val="%d"/>' % span) if span > 1 else ''
    tcpr = ('<w:tcPr><w:tcW w:w="%d" w:type="dxa"/>%s%s'
            '<w:tcMar><w:top w:w="50" w:type="dxa"/><w:left w:w="70" w:type="dxa"/>'
            '<w:bottom w:w="50" w:type="dxa"/><w:right w:w="70" w:type="dxa"/></w:tcMar>'
            '<w:vAlign w:val="center"/></w:tcPr>') % (w, gs, shd)
    p = ('<w:p><w:pPr><w:widowControl w:val="0"/>'
         '<w:spacing w:line="240" w:lineRule="auto"/><w:jc w:val="center"/>'
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

def insert_before_para(snippet, xml):
    global d
    i = d.find(esc(snippet))
    assert i > 0, "anchor not found: " + snippet[:60]
    ps = max(d.rfind("<w:p ", 0, i), d.rfind("<w:p>", 0, i))
    d = d[:ps] + xml + d[ps:]
    print("  [inserted before] " + snippet[:50])

# ================================================================
# S1.5 + S1.6  (before "S2 Stationary Instruments")
# ================================================================



S15 = (para([run("S1.5 Cross-laboratory consistency")], style="Heading2") +
  para([run("The two mobile laboratories normally drive different routes, so a direct side-by-side comparison is limited to occasions when both sampled the same 500 m grid cell on the same day: two such days occurred during the campaign, providing 29 shared cell-days (12 for HCN, which the EMU measured only from 2025). Comparing within-cell daily medians between vans, the median EMU/CAT ratio was 1.00 for benzene and 0.99 for toluene, with larger divergence for trimethylbenzene (0.70) and xylene (0.75), consistent with the vehicles' different audited MDLs for these compounds (Table S1.2). "),
       ] + [h2s()] + [
        run(" and HCN agreement is dominated by below-MDL values: median absolute differences (1.0 and 1.5 ppb) are of the same order as the detection limits themselves. Rank correlations across shared cells were modest (Spearman 0.17-0.45 excluding HCN), as expected when most paired values lie below detection (Figure S1.1). These comparisons use the public repository data; instrument-level colocation records, where available from CDPHE, would complement this field-level check.")]) +
  figure("FIG_cat_emu_comparison.png") +
  para([run("Figure S1.1: ", bold=True),
        run("Field-level cross-laboratory comparison. Each point is a 500 m grid cell sampled by both mobile laboratories on the same day; axes show the within-van daily median (log scale; zero-valued medians omitted from display). Red dashed line = 1:1.")]))

S16 = (para([run("S1.6 Within-campaign stability")], style="Heading2") +
  para([run("As an internal check on instrument stability in the absence of complete calibration records, Figure S1.2 shows monthly median and 95th-percentile concentrations per vehicle over the full campaign. Monthly medians are stable throughout, with no monotonic trend on either vehicle and no step change at the audit-period boundaries at which MDLs were re-determined (dotted lines; Table S1.2). Because the medians of most species are dominated by instrument background, sustained drift would appear directly in these series; its absence supports the comparability of measurements across the 29-month record.")]) +
  figure("FIG_monthly_stability.png") +
  para([run("Figure S1.2: ", bold=True),
        run("Monthly median (solid) and 95th-percentile (dashed) concentrations per mobile laboratory. Dotted vertical lines mark audit-period boundaries at which MDLs changed. Months with fewer than 1,000 valid observations are omitted.")]))

insert_before_para("S2 Stationary Instruments", S15 + S16)

# ================================================================
# S3 additions (before "S4 Evaluating General Concentrations")
# ================================================================
S3A = (para([run("Sensitivity to the treatment of below-MDL observations")], style="Heading3") +
  para([run("A large fraction of the 1-s observations lie below the audited method detection limits. Applying the vehicle- and period-specific MDLs published in the quarterly READ-ME files of the official CDPHE repository (https://www.colorado.gov/airquality/air_toxics_repo.aspx) to the reported concentrations flags 95.2% of benzene, 39.0% of toluene, 76.1% of trimethylbenzene, 55.5% of xylene, 97.5% of "), ] + [h2s()] + [
        run(", and 50.5% of HCN observations as below the MDL, consistent with the flag-based percentages in Table S3.1. To evaluate whether our conclusions depend on how below-MDL observations are treated, we recomputed key quantities under four cases: the raw reported concentrations used throughout this study, and substitution of every below-MDL value with 0, MDL/2, or the MDL. Notably, CDPHE's own quarterly data summaries substitute observations flagged within ±MDL with 0.5 × MDL and negative values with zero (repository READ-ME files); our MDL/2 case therefore reproduces the official summary convention, while the raw case retains the full reported signal. (The value-based HCN fraction is lower than the flag-based figure in Table S3.1 because the EMU's much lower audited MDL of 0.18 ppb applies to a large share of the post-January-2025 HCN record; the flag-based figure reflects the MDLs CDPHE applied at flagging time.)")]) +
  para([run("Campaign-level statistics for pollutants whose MDLs lie below typical ambient levels (toluene, xylene) are essentially unchanged across the four cases (Figure S3.11A). For pollutants measured mostly below the MDL (benzene, "), ] + [h2s()] + [
        run("), substitution mechanically bounds the summary statistics: the campaign median benzene is 0, 0.10, 0.60, and 1.20 ppb under the 0, raw, MDL/2, and MDL cases, respectively, and under full-MDL substitution the 95th percentile (3.2 ppb) reflects the largest quarterly MDL rather than any measurement.")]) +
  para([run("Substitution also degrades, rather than improves, the spatial information underlying the census-block comparison. Recomputing the block-level benzene metric (median of daily medians, unscaled) under each case across the 1,665 blocks that retain a valid unscaled benzene value under every substitution case (three fewer than the 1,668 blocks used for the scaled AirToxScreen comparison in Section S4), the Pearson correlation with the raw-value surface falls to 0.79 (substitute 0), 0.35 (MDL/2), and 0.01 (MDL), because substituted values track the detection limit in force at the time of sampling rather than ambient air; the median block-to-AirToxScreen ratio is correspondingly inflated from 0.66 (raw, unscaled) to 3.3 (MDL/2) and 6.6 (MDL), values far above any plausible ambient benzene level. Because CDPHE reports the measured signal even below the MDL, and because medians over large ensembles of noisy but unbiased measurements retain spatial information that fixed substitution destroys, we use the raw reported concentrations throughout, interpret sub-MDL medians as semiquantitative, and regard the 0- and MDL-substitution cases as bounding scenarios.")]) +
  figure("FIG_mdl_sensitivity.png") +
  para([run("Figure S3.11: ", bold=True),
        run("Sensitivity to the treatment of below-MDL observations. (A) Campaign median (bars) and 95th percentile (open points) for each pollutant under the four cases: raw reported values, and substitution of below-MDL values with 0, MDL/2, and MDL. (B) Distribution of the census-block benzene metric (median of daily medians, unscaled) under each case; the dashed red line marks the median AirToxScreen benzene concentration across the same blocks. Vehicle- and period-specific MDLs from the CDPHE audit values published in the repository READ-ME files.")]) +
  para([run("Figure S3.12 makes the spatial consequence of the detection limits explicit: for each pollutant, 500 m cells whose median (of daily medians, raw reported values) falls below the predominant audited MDL are shown in gray. The fraction of below-MDL cells is 20.6% for toluene, 47.3% for xylene, 71.9% for trimethylbenzene, 99.3% for benzene, and 100% for "), ] + [h2s()] + [
        run(" and HCN. The HCN panel of Figure 2 in the main text is therefore best read as a map of instrument background; toluene and xylene, whose MDLs sit well below ambient levels, retain nearly all of their spatial structure.")]) +
  figure("FIG_belowMDL_maps.png") +
  para([run("Figure S3.12: ", bold=True),
        run("Alternative presentation of the concentration maps with below-MDL cells grayed out. Cell values are medians of daily medians of raw reported concentrations on the 500 m grid; colored cells exceed the predominant audited MDL (benzene 0.5, toluene 0.18, xylene 0.19, trimethylbenzene 0.22, H2S 5, HCN 13 ppb; CAT laboratory values from the CDPHE repository READ-ME files), gray cells fall below it. Panel subtitles give the percentage of cells below the MDL. Color scales are clipped at the 2nd-98th percentiles of above-MDL cells.")]))

# ---- Table S3.2 health-reference block (values = patched 54 expected) ----
W32 = [1500, 1130, 1130, 1420, 1130, 1000, 1930, 1120]
rows32 = [row([
  cell([run("Pollutant", bold=True)], W32[0], fill=HDR),
  cell([run("Campaign median (ppb)", bold=True)], W32[1], fill=HDR),
  cell([run("p99 event threshold (ppb)", bold=True)], W32[2], fill=HDR),
  cell([run("Max sustained cell, 24-h scaled (ppb)", bold=True)], W32[3], fill=HDR),
  cell([run("EPA IRIS RfC (ppb)", bold=True)], W32[4], fill=HDR),
  cell([run("Chronic HQ", bold=True)], W32[5], fill=HDR),
  cell([run("ATSDR MRLs acute / interm. / chronic (ppb)", bold=True)], W32[6], fill=HDR),
  cell([run("Exceeds chronic MRL", bold=True)], W32[7], fill=HDR)])]
for vals in [
  ("Benzene","0.10","1.8","0.46","11.5","0.040","9 / 7 / 2 (draft 2024)","No"),
  ("Toluene","0.27","4.31","1.04","1,621","0.001","2,000 / - / 1,000","No"),
  ("Trimethylbenzene","0.15","2.59","0.38 (unscaled)","14.9","0.025","- / - / -","-"),
  ("Xylene","0.19","3.19","0.76","28.1","0.027","2,000 / 600 / 50","No"),
  ("H2S","0.25","4.8","2.45 (unscaled)","1.75","1.40*","70 / 20 / -","-"),
  ("HCN","1.0","11","1.5 (unscaled)","0.88","1.70*","- / - / -","-")]:
    rows32.append(row([cell([run(v)], W32[k]) for k, v in enumerate(vals)]))
S3B = (para([run("Table S3.2 places the campaign concentrations in the context of health-based reference values (a fuller cumulative assessment on the census-block basis, with organ-system hazard indices, is given in Section S7). Three observations follow. First, for the aromatics, sustained concentrations are well below non-cancer reference levels: chronic hazard quotients (the highest sustained 500 m cell median, scaled to 24-h equivalence where scaling factors exist, divided by the EPA IRIS RfC converted at the 830 hPa site pressure) are 0.040 (benzene), 0.001 (toluene), 0.025 (trimethylbenzene), and 0.027 (xylene); no aromatic cell approaches the ATSDR chronic minimal risk levels. Second, the nominal hazard quotients for "), ] + [h2s()] + [
        run(" (1.40) and HCN (1.70) must not be read as demonstrated exceedances: both are determined by values at or below the method detection limits, because the MDLs themselves (up to 5-6 ppb for "), ] + [h2s()] + [
        run(" and 13 ppb for HCN) lie well above the IRIS reference concentrations (1.75 and 0.88 ppb at site pressure). The correct conclusion is a measurement-capability gap: current mobile instrumentation cannot resolve ambient concentrations at the level of the health reference values for these two species. Third, the campaign 99th percentile for "), ] + [h2s()] + [
        run(" (4.8 ppb) falls within the commonly reported odor detection range (~0.5-8 ppb), consistent with the odor complaints that motivate community concern in the corridor. CDPHE health guideline values under Colorado's air-toxics rulemaking were not yet final at the time of writing and are not tabulated.")]) +
  para([run("Table S3.2: ", bold=True),
        run("Campaign concentrations compared with health-based reference values. The maximum sustained cell is the highest 500 m cell median of daily medians, scaled to 24-h equivalence for benzene, toluene, and xylene using the Section S4.1 factors. IRIS RfCs converted to ppb at 25 °C and the 830 hPa site pressure. *H2S and HCN hazard quotients are determined by values at or below the method detection limits and indicate a measurement-capability gap rather than demonstrated exceedances (see text). H2S odor detection threshold ~0.5-8 ppb. Campaign median and p99 event threshold are measured concentrations (Table S3.1); the maximum sustained cell uses the background-corrected concentrations that underlie the maps and Section 3.3. ATSDR MRLs as of 2024-2025; benzene values are from the October 2024 draft profile.")]) +
  table(W32, rows32))

S3C = (para([run("Wildfire smoke cross-check")], style="Heading3") +
  para([run("To test whether regional wildfire smoke influenced the campaign - of particular relevance for HCN, which is a biomass-burning tracer as well as a refinery FCC tracer - we classified every sampling day using the NOAA Hazard Mapping System (HMS) satellite smoke product: the maximum smoke-polygon density overlying the study domain on each day (none / light / medium / heavy). Of the 203 sampling days, 157 had no smoke overlay, 38 had light, 5 medium, and 3 heavy overlays, and an HMS product was retrievable for every day. Campaign-wide daily median concentrations were not elevated on smoke-affected days for any pollutant (Figure S3.13): benzene daily medians were unchanged (ratio 1.00), toluene and xylene were modestly lower on smoke days (ratios 0.78 and 0.66; Wilcoxon p = 0.012 and 0.006), consistent with the summer meteorology of the smoke season (deeper boundary layers) rather than any smoke enhancement, and "), ] + [h2s()] + [
        run(" medians were somewhat higher under light overlay (0.9 vs 0.4 ppb) but not significantly so (p = 0.27). For HCN, statistical power is limited because few smoke-overlay days fall within its post-January-2025 measurement record. Two caveats apply: HMS smoke is a column-integrated satellite product, so overhead smoke need not reach the surface; and the smoke-day list can be cross-checked against CDPHE air-quality advisories. On this evidence, wildfire smoke does not explain the elevated HCN background or any of the persistent hotspot signals.")]) +
  figure("FIG_smoke_comparison.png") +
  para([run("Figure S3.13: ", bold=True),
        run("Campaign-wide daily median concentrations by NOAA HMS smoke class (maximum smoke-polygon density overlying the study domain on each sampling day). Each box summarizes one class's sampling days. No pollutant shows elevation on smoke-affected days.")]))

S3D = (para([run("Seasonal patterns")], style="Heading3") +
  para([run("Sampling covered all seasons (48 DJF, 63 MAM, 52 JJA, and 40 SON days). Daily median concentrations of the aromatics are highest in winter (e.g., benzene 0.20 ppb in DJF vs 0.10 ppb in other seasons; toluene 0.38 vs 0.18-0.30 ppb), consistent with shallow wintertime boundary layers, and high-concentration (>p99) event rates are correspondingly elevated in DJF for the aromatics (Figure S3.14). "), ] + [h2s()] + [
        run(" shows no winter maximum in its native-cadence daily medians (0.37 ppb in DJF, 0.80 in MAM and JJA, 0.67 in SON), and its event rate peaks in autumn. The HCN record (January-June 2025 only) is strongly seasonal within its short span - a winter median of 2.75 ppb falling to 0.5-1 ppb in spring and summer - consistent with a boundary-layer-modulated background; this seasonality is confined to the 2025 record and does not affect the spatial analyses, which compare locations within the same days.")]) +
  figure("FIG_seasonal.png") +
  para([run("Figure S3.14: ", bold=True),
        run("Distributions of campaign-wide daily median concentrations by season. HCN is available from January 22, 2025 onward only.")]))

insert_before_para("S4 Evaluating General Concentrations", S3A + S3B + S3C + S3D)

# ================================================================
# S4.3 - S4.6  (before "S5 Hotspots")  -- values ⟦X⟧ filled from her run
# ================================================================
S43 = (para([run("S4.3 Sensitivity of the temporal scaling factor")], style="Heading2") +
  para([run("Mobile sampling occurred almost exclusively on weekday daytimes, so block-level concentrations are scaled to 24-h-equivalent values using the diurnal pattern at the La Casa site (section S4.1). To test the dependence of our conclusions on this construction, we recomputed the scaling factor five ways: (A) the baseline bin-weighted construction (La Casa 24/7 mean divided by the La Casa mean weighted by the mobile campaign's weekday × hour sampling distribution); (B) a simple window ratio (24/7 mean divided by the weekday 08:00-15:00 mean); (C) hour-of-day weights only, ignoring weekday; (D) a median-based analogue of the baseline; and (E) no scaling. Because the factor is a single multiplier applied to block concentrations and excess risk is linear in concentration, each construction propagates exactly - with no pipeline rerun - to the benzene risk range and the aggregate mobile-to-AirToxScreen ratio.")]) +
  para([run('The benzene factor spans 0.97-1.151 across constructions (A: 1.149; B: 1.101; C: 1.151; D: 0.973; E: 1.000), with toluene 1.11-1.36 and xylene 1.21-1.51. The resulting aggregate risk ratio ranges from 0.82 (median-based) to 0.97 (baseline and hour-only constructions), with mobile risk ranges of 0.096-0.403 excess cases (Figure S4.9). (The baseline reproduction here, 0.97, differs slightly from the main-text ratio of 0.92 because this sensitivity recomputes the aggregate directly from the population-weighted concentration surface, which is exactly linear in the factor; the comparison across constructions is internally consistent.) Under no construction does the aggregate mobile-derived risk exceed the AirToxScreen value: the central conclusions - aggregate risk comparable to the screening model, spatial allocation radically different - are insensitive to the scaling choice.')]) +
  figure("FIG_scaling_sensitivity.png") +
  para([run("Figure S4.9: ", bold=True),
        run("Sensitivity of the aggregate mobile-to-AirToxScreen benzene risk ratio to the construction of the La Casa temporal scaling factor. Bars show the ratio under the five constructions described in section S4.3; labels give the benzene scaling factor and the resulting mobile risk range (excess lifetime cancer cases across the 1,668 common blocks). The red dashed line marks parity with AirToxScreen (0.117-0.416 cases). Risk scales exactly linearly with the factor.")]))

S44 = (para([run("S4.4 Day/night and weekday/weekend contrasts at La Casa")], style="Heading2") +
  para([run('To characterize the periods the mobile campaign could not sample, we compared La Casa concentrations in the mobile driving window (weekdays 08:00-15:59) with nights (20:00-05:59, all days) and weekend daytimes (Sat-Sun 06:00-19:59). Nighttime mean concentrations exceed the driving-window means by factors of 1.24 (benzene), 1.38 (toluene), and 1.55 (xylene) (median-based ratios 1.01, 1.53, and 1.77), consistent with the shallow nocturnal boundary layer concentrating emissions. Weekend daytime means are 8-19% lower than weekday daytimes (ratios 0.92, 0.81, and 0.85), consistent with reduced traffic and industrial activity (Figure S4.10).')]) +
  para([run("Two implications follow. First, the unsampled periods are, on average, higher-concentration periods: daytime-only mobile estimates understate, rather than overstate, 24-h exposure, and the bin-weighted scaling factors (section S4.1) fall between the driving-window and nighttime levels because they integrate both the elevated nights and the slightly lower weekends into a single 24/7 correction. Second, these ratios reflect the regional temporal pattern at a single stationary site; source-specific nighttime behavior (e.g., episodic industrial releases) cannot be constrained by this campaign and is flagged as a limitation in the main text.")]) +
  figure("FIG_lacasa_diurnal.png") +
  para([run("Figure S4.10: ", bold=True),
        run("Hourly mean concentrations at the La Casa stationary site for weekdays (blue) and weekends (red); ribbons show the interquartile range and the shaded band marks the mobile campaign's weekday driving window (08:00-15:59). Nighttime concentrations exceed driving-window levels, so the daytime mobile window samples the cleaner part of the diurnal cycle.")]))

S45 = (para([run("S4.5 Day-resampling uncertainty for the block comparison")], style="Heading2") +
  para([run("To quantify the sampling uncertainty of the census-block comparison, we resampled sampling days with replacement (500 replicates), recomputing each block's benzene median of daily medians (raw reported values, scaled by the Section S4.1 factor) and the population-weighted aggregate mobile-to-AirToxScreen ratio (unit risk factors cancel). The aggregate ratio is 0.92 with a 95% bootstrap interval of 0.78-1.06: aggregate parity with the screening model is robust to which days happened to be sampled. Block-level exceedances are more day-sensitive, as expected for localized signals observed on a limited number of visits: of the 94 blocks whose point estimate exceeds twice the AirToxScreen value in this construction, 6 remain above 2x in at least 80% of bootstrap replicates and 1 in at least 95% (Figure S4.11). The correct reading is that the existence of a heavy upper tail of block-level exceedances is robust, while the identity of individual exceeding blocks carries day-sampling uncertainty - reinforcing our emphasis on persistent, recurring hotspots rather than single-block values. (Block counts here differ modestly from the main-text figures because this bootstrap uses raw rather than background-corrected concentrations.)")]) +
  figure("FIG_bootstrap_blocks.png") +
  para([run("Figure S4.11: ", bold=True),
        run("Day-resampling bootstrap (500 replicates). (A) Distribution of the population-weighted aggregate mobile-to-AirToxScreen benzene ratio; red dashed line marks parity. (B) For blocks whose point estimate exceeds 2× AirToxScreen, the bootstrap probability of remaining above 2×.")]))

S46 = (para([run("S4.6 Environmental-justice context")], style="Heading2") +
  para([run("Colorado EnviroScreen (v2, November 2024 release) scores every census block group in the state on a combined index of environmental exposures, environmental effects, climate vulnerability, health, and socioeconomic burden, and designates disproportionately impacted (DI) communities under Colorado's environmental-justice statute. We joined each of the 1,668 analyzed census blocks to its parent block group (all matched) and compared blocks where mobile-derived benzene exceeds AirToxScreen by more than a factor of two against the remainder.")]) +
  para([run("The analyzed domain is heavily burdened overall: the median block lies at the 86th percentile of the statewide EnviroScreen distribution, and 81% of blocks fall in designated DI block groups (most commonly through the 'more than one category' designation, followed by 'people of color population above 40%'). Against this background, the 98 blocks where mobile-derived benzene exceeds AirToxScreen by more than a factor of two are statistically indistinguishable from the rest of the domain in burden: median EnviroScreen percentile 84.8 versus 86.4 (Wilcoxon p = 0.96) and 80% versus 81% located in DI communities (Figure S4.12). We therefore do not claim a within-domain environmental-justice gradient in the mobile-versus-model discrepancy. The policy-relevant statement is the aggregate one made in Section 3.3: because the monitoring domain is itself composed almost entirely of disproportionately impacted communities, the localized exposures that the screening model does not resolve fall overwhelmingly on populations already designated as over-burdened.")]) +
  figure("FIG_ej_overlay.png") +
  para([run("Figure S4.12: ", bold=True),
        run("Colorado EnviroScreen v2 percentile scores for census blocks where mobile-derived benzene exceeds AirToxScreen by more than a factor of two, compared with all other analyzed blocks. Higher percentiles indicate greater cumulative environmental and social burden. The two distributions are statistically indistinguishable, and both sit far above the statewide median, reflecting the burden of the monitoring domain as a whole.")]))

insert_before_para("S5 Hotspots", S43 + S44 + S45 + S46)

# ================================================================
# S5.3 - S5.6  (before "S6 Constraining Emissions")
# ================================================================
S53 = (para([run("S5.3 Sensitivity to clustering and persistence parameters")], style="Heading2") +
  para([run("The hotspot identification chain has three tuning parameters: the event threshold (observations above the campaign 99th percentile of each pollutant), the DBSCAN spatial radius (eps = 100 m, minPts = 1), and the persistence criterion (clusters in the top decile of both observation count and distinct sampling days). We reran the full chain - event selection, per-pollutant clustering, persistence screening, and cross-pollutant grouping (at the same eps) - under a complete 3 × 3 × 3 factorial: event thresholds p98.5, p99, and p99.5; eps of 50, 100, and 200 m; and persistence percentiles p85, p90, and p95 (27 variants). For each variant we recorded the number of persistent clusters, the number of multi-pollutant groups, and a location-recovery metric: the fraction of the 17 baseline group centroids lying within 300 m of a variant group persistent in three or more pollutants.")]) +
  para([run("The number of groups persistent in ≥3 pollutants ranges from 0 (coarsest eps with strictest persistence) to 66 (finest eps with loosest persistence) and varies systematically with the clustering radius and persistence percentile, as expected for a screening rule: looser thresholds and finer clustering radii fragment the same activity into more, smaller groups (Figure S5.6). Location recovery is high across moderate perturbations - 41-94% of the 17 baseline locations are recovered for eps of 50-100 m with persistence p85-p90, regardless of event threshold - and degrades only in the most aggressive corners (eps = 200 m or persistence p95), where few clusters survive screening. The baseline configuration itself reproduces 17 groups, with 94% of the baseline centroids recovered within 300 m upon regrouping.")]) +
  para([run("These results indicate that the specific count of 17 groups is a property of the chosen screening stringency rather than a physical constant, but that the existence and locations of the major multi-pollutant hotspots are robust: the same core locations reappear across parameterizations, gaining or shedding satellite clusters as the thresholds move.")]) +
  figure("FIG_dbscan_sensitivity.png") +
  para([run("Figure S5.6: ", bold=True),
        run("Sensitivity of the multi-pollutant hotspot identification to its tuning parameters. Columns: event threshold (campaign percentile defining high-concentration observations). X-axis: DBSCAN eps. Colors: persistence percentile. Top row: number of groups persistent in three or more pollutants; bottom row: percentage of the 17 baseline group locations recovered within 300 m. The black circle marks the baseline configuration (p99, 100 m, p90).")]))

S54 = (para([run("S5.4 Fixed-site directional analysis at La Casa")], style="Heading2") +
  para([run("A conditional probability function (CPF) at the La Casa stationary site - the probability that benzene, toluene, or xylene exceeds its site-specific 90th percentile, conditional on wind sector, using La Casa's own meteorology (winds > 1 m/s; 142,639 valid rows) - peaks strongly for southwesterly to westerly winds (CPF 0.15-0.21), the direction of downtown Denver and the I-25/I-70 interchange, and is near its base rate (0.04-0.05) for the northeasterly sectors containing the industrial corridor (bearings 45-70 degrees; Figure S5.7). A fixed site 8-10 km from the corridor is thus dominated by nearer urban sources and registers little corridor influence - an independent, data-driven illustration of why fixed-site networks under-detect localized industrial impacts and why the mobile campaign was necessary. The corridor source regions identified by the mobile back-projection (Figure 3) are consistent with this picture: they are resolvable only by measurements taken near the corridor itself.")]) +
  figure("FIG_lacasa_cpf.png") +
  para([run("Figure S5.7: ", bold=True),
        run("Conditional probability of exceeding the site 90th percentile at La Casa by wind sector (16 sectors, La Casa meteorology, winds > 1 m/s). Red dashed radials mark bearings from La Casa to Sinclair (~45°), WWTF1 (~53°), Suncor (~62°), and Phillips 66 (~70°).")]))

S55 = (para([run("S5.5 Split-sample reproducibility")], style="Heading2") +
  para([run("As an internal validation of the persistent hotspot groups, we repeated the entire identification chain - within-half p99 event thresholds, DBSCAN clustering (eps = 100 m), top-decile persistence screening, and cross-pollutant grouping - independently on two splits of the campaign: odd versus even sampling days (interleaved, controlling for season; 102 and 101 days), and 2023-2024 (157 days) versus 2025 (46 days). The odd and even halves identified 20 and 16 groups persistent in three or more pollutants, respectively; 40% and 56% of one half's groups lie within 300 m of the other's, and the halves recover 82% and 53% of the 17 full-campaign group locations. The calendar halves identified 14 (2023-2024) and 17 (2025) groups, recovering 53% and 65% of the full-campaign locations, with cross-half agreement of 43% and 35%.")]) +
  para([run("Two conclusions follow. First, roughly half to four-fifths of the full-campaign group locations reappear in any given half-campaign (53-82% across the four halves), despite the halved sample size and independently recomputed thresholds - the major locations are recoverable from half the data, but the specific inventory is not. Second, recovery is comparable for the interleaved and calendar splits (82%/53% versus 53%/65%), so we do not find that balancing seasonal coverage materially improves reproducibility; several 2025-only groups have no 2023-2024 counterpart, indicating genuine temporal evolution of sources. Persistent-hotspot identification therefore benefits from sustained multi-season sampling, and group inventories should be interpreted as period-specific (Figure S5.8).")]) +
  figure("FIG_split_sample_hotspots.png") +
  para([run("Figure S5.8: ", bold=True),
        run("Split-sample reproducibility of the persistent multi-pollutant hotspot groups. Colored points show groups persistent in three or more pollutants identified independently within each half-campaign using identical parameters (within-half p99 thresholds, eps = 100 m, persistence p90); X symbols mark the 17 full-campaign groups. Left: odd versus even sampling days; right: 2023-2024 versus 2025.")]))

S56 = (para([run("S5.6 Sampling sufficiency")], style="Heading2") +
  para([run("To characterize how many sampling days this design requires, we drew random subsets of k days (k = 10-200; 20 draws for maps, 10 for hotspots) and recomputed (a) the 500 m benzene cell medians (Spearman correlation with the full-campaign map, cells with at least three sampled days) and (b) the full hotspot identification chain (within-subset thresholds), scoring recovery of the 17 full-campaign groups within 300 m. Map structure stabilizes first: the correlation reaches 0.76 by 40 days, 0.83 by 80, 0.93 by 160, and 0.99 by 200. Hotspot inventories converge much more slowly - 12% recovery at 10 days, 50-53% at 60-80, 68% at 120, 88% at 160, and 94% at 200 - because persistence screening requires repeated within-subset detections (Figure S5.9). Roughly two months of repeated weekday driving thus suffices to map general concentration patterns, but a persistent multi-pollutant hotspot inventory of the kind reported here requires a sustained, multi-season campaign - a concrete design guideline for agencies replicating this program.")]) +
  figure("FIG_sampling_sufficiency.png") +
  para([run("Figure S5.9: ", bold=True),
        run("Sampling-sufficiency curves. Left: Spearman correlation between subsampled and full-campaign benzene cell medians. Right: fraction of the 17 full-campaign hotspot groups recovered within 300 m by subsets of k days. Lines: medians over draws; ribbons: 10th-90th percentiles.")]))

insert_before_para("S6 Constraining Emissions", S53 + S54 + S55 + S56)

# ================================================================
# S6.6 - S6.8  (before the S7 hazard section)
# ================================================================
S66 = (para([run("S6.6 Minimum detectable emission rate")], style="Heading2") +
  para([run("To characterize the sensitivity of the plume method, we propagated the "), ] + [h2s()] + [
        run(" method detection limit through the same Gaussian formulation used for the inversion (Pasquill-Gifford dispersion coefficients, five-term vertical solution with reflections, effective source height H = 12.2 m, receptor height z = 1.5 m, centerline receptor). The minimum detectable emission rate Q"), run("min", sub=True),
        run(" is defined as the continuous emission rate that would produce a peak enhancement equal to the MDL at the mobile platform. Because the inversion is linear in the observed enhancement, Q"), run("min", sub=True),
        run(" is obtained by evaluating the inversion at ΔH"), run("2", sub=True), run("S = MDL. We evaluated Q"), run("min", sub=True),
        run(" on a grid of downwind distance (0.5-4 km), stability class (B, C, D), and wind speed (2-5 m/s) for MDL values of 4, 5, and 6 ppb, spanning the audited Picarro G2204 detection limits on the two vehicles (Table S1.2).")]) +
  para([run("Detectability degrades steeply with distance and with convective mixing (Figure S6.6). Under neutral (D) stability at 3.2 m/s, Q"), run("min", sub=True),
        run(" is approximately 9 t/yr at 0.5 km, 30 t/yr at 1 km, and 96 t/yr at 2 km (MDL = 5 ppb); under convective (B) conditions, vertical dilution raises Q"), run("min", sub=True),
        run(" to approximately 720 t/yr at 2 km. At the four retained plumes' actual distances, wind speeds, stability classes, and boundary-layer depths, Q"), run("min", sub=True),
        run(" (MDL = 5 ppb) was 358, 408, 675, and 701 t/yr, and the ratio of the observed peak enhancement to the MDL was 1.0-2.8: three of the four retained plumes were detected essentially at the detection threshold.")]) +
  para([run("Two implications follow. First, the inferred emission range (471-1,964 metric t/yr) is censored from below: continuous releases smaller than the distance- and stability-dependent minimum - including rates well above zero - would not have produced detectable enhancements at the sampled distances, so the retained events represent the detectable upper tail of release conditions rather than a representative sample of facility operation. Second, the method's sensitivity improves markedly close to the source and in neutral-to-stable conditions (Q"), run("min", sub=True),
        run(" of roughly 5-14 t/yr at 0.5 km under D stability across 2-5 m/s), indicating that future targeted sampling nearer the facility fence line, or with the lower EMU detection limit (2 ppb in some quarters), could constrain substantially smaller releases.")]) +
  figure("FIG_min_detectable_rate.png") +
  para([run("Figure S6.6: ", bold=True),
        run("Minimum detectable H2S emission rate as a function of downwind distance, stability class, and wind speed, obtained by propagating the H2S detection limit through the Gaussian plume formulation used for the WWTF inversion. Solid lines use MDL = 5 ppb; shaded bands span MDL = 4-6 ppb. Sources emitting below the curve for their distance and meteorology would not have been detectable.")]))

# ---- S6.7 permit record + Table S6.1 ----
W61 = [2340, 1560, 1980, 1900, 1580]
rows61 = [row([
  cell([run("Facility", bold=True)], W61[0], fill=HDR),
  cell([run("Identifier", bold=True)], W61[1], fill=HDR),
  cell([run("Reported H2S", bold=True)], W61[2], fill=HDR),
  cell([run("Reported HCN", bold=True)], W61[3], fill=HDR),
  cell([run("Source (year)", bold=True)], W61[4], fill=HDR)])]
for vals in [
  ("Suncor Energy refinery","TRI 80022CNCDN5801B","5,819 lb/yr (2.6 t/yr): 1,202 fugitive + 4,617 stack","22,373 lb/yr (10.1 t/yr), all stack","EPA TRI (2023)"),
  ("Metro Water Recovery - Robert W. Hite Treatment Facility (WWTF1)","CDPHE permit 95OPAD072; not a TRI reporter","Not quantified: wastewater treatment process exempt from permitting (Reg. 3, Part B II.D.1.d)","Not reported","CDPHE permit record"),
  ("Metro Water Recovery - Northern Treatment Plant (process analogue; same operator, processes described as 'very similar')","CDPHE permit 12WE2479, AIRS 123/99ED","340 lb/yr (0.17 t/yr) from flare (2% uncombusted); ~8 t/yr H2S contained in flared digester gas","Not reported","CDPHE APEN record"),
  ("Phillips 66 Denver Terminal","TRI 80022PHLLP3960E","Not reported","Not reported","EPA TRI (2023); reports aromatics only"),
  ("Sinclair (Holly Energy Partners) Denver Products Terminal","No TRI record retrieved","Not reported","Not reported","EPA TRI search (2023)"),
  ("This study - inverse Gaussian plume estimate for the WWTF-attributed plumes","-","471-1,964 metric t/yr (mean ~1,036)","-","Section 3.6; SI S6")]:
    rows61.append(row([cell([run(v)], W61[k]) for k, v in enumerate(vals)]))

S67 = (para([run("S6.7 Comparison with the facility permit record")], style="Heading2") +
  para([run("Reviewer comments asked how the inferred emission rates compare with facility-reported values. We therefore examined the Colorado air-permit record for Metro Water Recovery through CDPHE's public records portal. The operator holds two air permits: 95OPAD072 for the Robert W. Hite Treatment Facility (RWHTF), the facility nearest our plume intercepts, and 12WE2479 (AIRS 123/99ED) for its Northern Treatment Plant (NTP) in Brighton. The NTP file is the more completely documented of the two and, by the operator's own account in its permit application, the two plants are directly comparable: the submittal states that the RWHTF's liquid and solid stream processes are 'very similar' to those at the NTP, with capture of digester gas controlled by flares and co-generation engines, and derives several NTP emission factors from RWHTF values.")]) +
  para([run("Three features of this record bear on our results. First, and most importantly, the permits do not cover the processes in which "), ] + [h2s()] + [
        run(" is generated: the application states that 'the wastewater treatment process is permit exempt by Regulation 3, Part B II.D.1.d.' Permitted and reported emissions at these facilities are therefore confined to combustion and ancillary sources - the co-generation engines, flare, boilers, an emergency generator, and fugitive VOC from wastewater handling - and no permitted or reported "), ] + [h2s()] + [
        run(" value exists for the open liquid-phase and sludge-handling operations that are the principal route by which dissolved sulfide is stripped to the atmosphere. Second, the "), ] + [h2s()] + [
        run(" quantities that do appear in the permit record are small: the NTP reports 340 lb/yr (0.17 tons/yr) of "), ] + [h2s()] + [
        run(" from the flare, calculated as 2% of the sulfide passing through uncombusted, and the total "), ] + [h2s()] + [
        run(" contained in the flared digester gas is approximately 8 tons/yr at the design concentration of 2,500 ppmv. Digester-gas "), ] + [h2s()] + [
        run(" is reported as approximately 2,000-2,500 ppmv by design, measured below 300 ppmv in recent operation, and treated to below 10 ppmv with an iron sponge before the co-generation engine. Third, several of the underlying factors are dated: the liquid-process emission factor of 0.02 mg/L applied to these facilities was derived in 1992 from a Bay Area Sewage Toxics Emissions model run for the RWHTF.")]) +
  para([run("Our inferred rates of 471-1,964 metric tons/yr therefore exceed anything in the permit record for a comparable facility by two to four orders of magnitude. We do not interpret this as evidence of misreporting, and two readings are consistent with the evidence. The first is that the dominant "), ] + [h2s()] + [
        run(" pathway at a large wastewater facility - volatilization from open liquid-phase and solids-handling operations - is categorically exempt from permitting and hence never quantified, so the permit record provides no upper bound on actual emissions and cannot be used to falsify the measurements. The second is that the inversion overestimates: it assumes continuous operation at the rate implied by each intercept, treats the peak enhancement as a centerline value, and rests on four retained plumes, three of which sit at the detection threshold (Section S6.6). The available evidence does not distinguish these readings, and both support the manuscript's broader argument that permit and inventory records are an incomplete basis for assessing community exposure to air toxics: whichever holds, there is at present no reported quantity against which an ambient measurement of "), ] + [h2s()] + [
        run(" near these facilities can be checked.")]) +
  para([run("Table S6.1 assembles the reported quantities we were able to locate for facilities in the study domain. Two points stand out. First, the only facility in the domain that reports "), ] + [h2s()] + [
        run(" at all is the Suncor refinery, at 5,819 lb/yr (2.6 metric tons/yr) for 2023 - itself two to three orders of magnitude below our inferred plume-derived rates, so the discrepancy is not resolved by re-attributing the plumes from the wastewater facility to the refinery. Second, Suncor reports 22,373 lb/yr (10.1 metric tons/yr) of HCN, essentially all from stacks, which is consistent with fluid catalytic cracking as the dominant regional HCN source and corroborates the source attribution in Section 3.4. Neither petroleum products terminal reports "), ] + [h2s()] + [
        run(" or HCN, and Metro Water Recovery is not a TRI reporter, wastewater treatment not being a TRI-covered sector.")]) +
  para([run("Table S6.1: ", bold=True),
        run("Reported hydrogen sulfide and hydrogen cyanide emissions for facilities in the study domain, compared with the plume-inversion estimate. TRI values are on-site air releases for reporting year 2023 retrieved from EPA Envirofacts; permit values are from the CDPHE public records portal. 'Not reported' indicates the pollutant does not appear in the facility's filing; 'not quantified' indicates that the emitting process is categorically exempt from the permitting programme and therefore has no reported value.")]) +
  table(W61, rows61))

# ---- S6.8 attribution + Table S6.2 ----
W62 = [1420, 1000, 900, 800, 1000, 1240, 1200, 800, 1000]
rows62 = [row([
  cell([run("Date & time (local)", bold=True)], W62[0], fill=HDR),
  cell([run("Dist. from WWTF (km)", bold=True)], W62[1], fill=HDR),
  cell([run("Peak ΔH2S (ppb)", bold=True)], W62[2], fill=HDR),
  cell([run("Stability", bold=True)], W62[3], fill=HDR),
  cell([run("Emission (t/yr)", bold=True)], W62[4], fill=HDR),
  cell([run("Min. detectable, MDL 4-6 (t/yr)", bold=True)], W62[5], fill=HDR),
  cell([run("Aromatic/HCN co-plume", bold=True)], W62[6], fill=HDR),
  cell([run("ΔCH4 (ppm)", bold=True)], W62[7], fill=HDR),
  cell([run("Attribution", bold=True)], W62[8], fill=HDR)])]
for vals in [
  ("2023-04-26 09:20","4.11","6.0","C","706","326-490","None","0.094","WWTP-consistent"),
  ("2023-11-16 14:21","1.95","14.0","B","1,964","540-810","None","- (no record)","WWTP-consistent"),
  ("2024-01-11 10:15","3.55","5.2","D","471","287-430","None","0.020","WWTP-consistent"),
  ("2024-05-09 10:59","4.30","5.0","C","1,003","560-841","None","0.038","WWTP-consistent")]:
    rows62.append(row([cell([run(v)], W62[k]) for k, v in enumerate(vals)]))

S68 = (para([run("S6.8 Source attribution of the retained H2S plumes")], style="Heading2") +
  para([run("Table S6.2 lists, for each of the four retained "), ] + [h2s()] + [
        run(" plumes, the intercept geometry and inferred emission rate together with the two-tier chemical attribution described in Section 2.5.5 of the main text. The fingerprint gate flags any co-located aromatic (benzene, toluene, xylene, trimethylbenzene) or HCN plume at the same three-standard-deviation threshold used for "), ] + [h2s()] + [
        run("; the methane column reports the co-located enhancement above the local rolling background. All four plumes are aromatic- and HCN-free (wastewater-consistent, not refinery); co-located methane enhancements are small (≤0.09 ppm) and do not independently corroborate. Emission rates are reported at baseline assumptions with the minimum detectable rate spanning audited "), ] + [h2s()] + [
        run(" detection limits of 4-6 ppb.")]) +
  para([run("Table S6.2: ", bold=True),
        run("Source attribution of the four retained H2S plumes (fingerprint gate + methane cross-check).")]) +
  table(W62, rows62))

insert_before_para("S7 Screening-Level Cumulative Noncancer Hazard Assessment", S66 + S67 + S68)

# ================================================================
# S8 Methane  (before References)
# ================================================================
S8 = (para([run("S8 Methane Measurements and Hotspots")], style="Heading1") +
  para([run("The Picarro G2204 instruments on both mobile laboratories also recorded methane (CH"), run("4", sub=True),
        run(") at 1 s resolution. Methane data were obtained directly from CDPHE (they are not part of the public air-toxics repository packets) and were processed with the same pipeline as the air toxics: timestamps were converted from UTC to local time, the asset-specific inlet delays measured for the Picarro (21 s for the CAT and 17 s for the EMU; section S1) were applied, observations within 100 m of the ATOPs headquarters were removed to exclude garage and idling periods, and wind data were assigned from the nearest EPA monitoring site (90.3% joined after an hourly fallback). The resulting record comprises 1,837,632 1-s observations on 193 sampling days on the two North Denver/Commerce City routes.")]) +
  para([run("The methane channel was not routinely calibrated during the campaign. A single field check recovered 96.4-97.8% of a 5 ppm standard, suggesting a modest low bias. We therefore interpret methane concentrations in relative terms, and use them for spatial pattern and source identification rather than absolute quantification.")]) +
  para([run("Applying the same hotspot algorithm used for the air toxics (section S5), high-methane events were defined as 1-s observations at or above the campaign-wide 99th percentile (2.575 ppm; the 95th percentile was 2.283 ppm). The 18,378 high-methane events (observed on 160 of 193 days) grouped into 66 spatial clusters (DBSCAN, eps = 100 m, minPts = 5), of which two met the persistence criteria used for the air toxics (clusters containing >10% of all high-methane events and observed on ≥16 event days). The largest persistent methane hotspot (39.8233° N, 104.9741° W; 11,843 events on 66 days; maximum 19.3 ppm) lies in the refinery-adjacent industrial corridor, and the second (39.7818° N, 104.9860° W; 2,841 events on 57 days; maximum 10.9 ppm) lies along the Vasquez Boulevard corridor. Both are more than 2.6 km from the nearest persistent multi-pollutant air-toxics hotspot group, consistent with methane sources (e.g., natural gas infrastructure, waste handling, and fugitive refinery emissions) that are distinct from the sources driving the air-toxics hotspots.")]) +
  para([run("Figure S8.1 shows the upwind source-probability surface for high-methane events, constructed identically to the air-toxics surfaces in section 2.5.3.1 of the main text (events ≥ 99th percentile with wind speed > 1 m/s, 15 km upwind rays, 150 m discretization, concentration-weighted with weights capped at 5, exponential distance kernel, and Gaussian smoothing). The surface places the most likely upwind source region to the northeast of the sampling domain (maximum at 39.872° N, 104.876° W), a sector containing agricultural operations and oil and gas activity, with a secondary ridge along the industrial corridor.")]) +
  figure("methane_sourceprob_map.png") +
  para([run("Figure S8.1: ", bold=True),
        run("Methane source-probability surface based on 99th-percentile methane events and 15 km upwind rays, constructed and styled identically to Figure 3 of the main text. Values are normalized to the domain maximum.")]) +
  figure("FIG_methane_at_toxics_hotspots.png") +
  para([run("Figure S8.2: ", bold=True),
        run("Methane at the 17 persistent air-toxics hotspot groups. Top: for each group, the percentage of 1-s methane observations within 100 m of the group centroid at or above the campaign-wide 95th and 99th percentiles, the number of sampling days with high-methane events, and the median methane mixing ratio. Bottom: hotspot groups colored by methane class (enriched / intermediate / quiet) together with the 66 methane event clusters (grey; triangles mark the two persistent methane hotspots). Methane co-elevation separates gas-associated hotspot groups from combustion- and evaporation-driven groups.")]))

i = d.find(">References<")
ps = d.rfind("<w:p ", 0, i)
d = d[:ps] + S8 + d[ps:]
print("  [inserted] S8 Methane before References")

# ================================================================
# placeholder check + write
# ================================================================
assert "⟦" not in d, "unfilled placeholders remain: " + ", ".join(set(re.findall(r'⟦[^⟧]+⟧', d)))
open(P, "w", encoding="utf-8").write(d)
open(RELS, "w", encoding="utf-8").write(rels)
import shutil
for src, name in _media_added:
    shutil.copy(src, os.path.join("unpacked", "word", "media", name))
print("document.xml rewritten (%d bytes); %d media files added" % (len(d), len(_media_added)))
