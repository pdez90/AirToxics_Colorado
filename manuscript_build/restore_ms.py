# -*- coding: utf-8 -*-
"""Restore the prose lost in the 2026-08-21 rebuild, from the 2026-08-20 working
draft (Toxics_EST/MobileToxics_CDPHE_UPDATED.docx), with every number updated to
the current (2026-08-21/22) pipeline run. All restored text is red.

Every numeric claim in the restored text was re-derived on 2026-08-23:
  752 TRI facilities (FIG_G log) | 1,668 blocks / 126,607 residents (block RData)
  delays 4/6/21 CAT, 5/3/17 EMU | CAT-EMU 29 cell-days 1.00/0.99/0.70/0.75 (FIG_R)
  p99s 1.8/4.31/2.59/3.19/4.8/11 (Table S3.1) | funnel 2,713 -> 216 persistent
  (35/33/32/35/58/23) -> 151 candidate -> 37 ge2 / 17 ge3 / 8 ge4 (thresholds CSV,
  group_summary 151 rows, MASTER 17) | 47-grid recovery 24-94% single-step (FIG_P)
  magnitude: 98 blocks >2x (5.9%), 18 >5x, max 17.8x, ATS 0.124-0.309, mobile 2.3
  EJ: joined 1668, median pctl 86.4, DI 81.3%; >2x subset 84.8/79.6% vs 86.4/81.4%,
  Wilcoxon p=0.96, pop 7,420 (EnviroScreen GEOID join, container)
  composition: benzene 13/17, H2S 5, HCN 2, five-pollutant 9/12/34 (MASTER)
  Group 9: TMB/B 3.24 max of 17, enrichment max 0.96, TRI dist 1.03 km,
  CH4 36.8% / 11 days (methane_at_toxics_hotspots.csv)
  attribution: 4 retained plumes aromatic/HCN-free, dCH4 <=0.094 ppm, 1.95-4.30 km
  methane: 1,837,632 obs / 193 days, p99 2.57 ppm, 66 clusters, 2 persistent
  (39.823N 104.974W 66 days max 19.3 ppm; 39.782N 104.986W 57 days), >2.6 km,
  sourceprob max 39.872N 104.876W (M01-M06 logs) | 10 of 17 groups <5% CH4
"""
import redfix

P = "unpacked/word/document.xml"
d = open(P, encoding="utf-8").read()

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

def para(runs, style=None):
    ppr = "<w:pPr>"
    if style: ppr += '<w:pStyle w:val="%s"/>' % style
    ppr += "<w:rPr/></w:pPr>"
    return "<w:p>%s%s</w:p>" % (ppr, "".join(runs))

def cite(n): return run(n, sup=True)
def h2s(): return run("H") + run("2", sub=True) + run("S")

def para_end_after(snippet):
    """Return index just past the </w:p> of the paragraph containing snippet."""
    i = d.find(esc(snippet))
    assert i > 0, "anchor not found: " + snippet[:60]
    return d.find("</w:p>", i) + len("</w:p>")

def para_start_before(snippet):
    i = d.find(esc(snippet))
    assert i > 0, "anchor not found: " + snippet[:60]
    ps = d.rfind("<w:p ", 0, i)
    ps2 = d.rfind("<w:p>", 0, i)
    return max(ps, ps2)

def insert_after(snippet, xml):
    global d
    j = para_end_after(snippet)
    d = d[:j] + xml + d[j:]

def insert_before(snippet, xml):
    global d
    j = para_start_before(snippet)
    d = d[:j] + xml + d[j:]

def replace_para(snippet, xml, label=""):
    """Replace the whole paragraph containing snippet with xml."""
    global d
    i = d.find(esc(snippet))
    assert i > 0, "replace anchor not found: " + snippet[:60]
    ps = max(d.rfind("<w:p ", 0, i), d.rfind("<w:p>", 0, i))
    pe = d.find("</w:p>", i) + len("</w:p>")
    d = d[:ps] + xml + d[pe:]
    print("  [replaced para] %s" % (label or snippet[:60]))

# ================================================================
# 1. INTRODUCTION — replace the five submitted paragraphs with the
#    2026-08-20 rewrite; insert the mobile-monitoring-niche paragraph;
#    replace the contributions paragraph.
# ================================================================
replace_para("Section 112 of the U.S. Clean Air Act (CAA) identifies 188 air toxics",
  para([run("Section 112 of the U.S. Clean Air Act (CAA) identifies 188 air toxics, or hazardous air pollutants (HAPs), known or suspected to cause cancer or reproductive, developmental, and neurological harm."), cite("1"),
        run(" Although HAPs share sources with criteria pollutants, the two are regulated differently: criteria pollutants are governed by national ambient air quality standards, whereas air toxics are controlled primarily through technology-based emissions limits, with limited consideration of ambient concentrations."), cite("2")]),
  "intro CAA")

replace_para("Listed HAPs encompass a variety of metals",
  para([run("HAPs include metals and volatile and semi-volatile organic compounds emitted by vehicles, industrial facilities, gas stations, dry cleaners, and wildfires. The existing observation network - roughly 100 state monitors and 30 National Air Toxics Trends Sites - measures only a subset of HAPs and is not designed to resolve exposures at the local scale (<100 km"), run("2", sup=True),
        run("), making it difficult to assess inhalation health risks. Data gaps are commonly filled with modeled estimates derived from emissions inventories."), cite("3,4"),
        run(" EPA's Air Toxics Screening Assessment (AirToxScreen), for example, blends dispersion and chemical transport modeling to estimate ambient concentrations."), cite("5"),
        run(" Such estimates inherit omissions and uncertainties in the underlying inventories."), cite("6")]),
  "intro HAPs/network")

replace_para("In 2021, the state of Colorado enacted legislation (HB21-1189)",
  para([run("In 2021, Colorado enacted HB21-1189, which names three “covered air toxics”—hydrogen cyanide, hydrogen sulfide, and benzene—and requires monitoring of them within three miles of “covered facilities”, with authority for the Air Quality Control Commission to add further hazardous air pollutants and facility categories by rule. Covered facilities are stationary sources in three industrial categories: petroleum refineries (NAICS 324110), other aircraft parts and auxiliary equipment manufacturing (336413), and petroleum bulk stations and terminals (424710), the last only where the source lies within an eight-hour ozone control area and reported benzene emissions to the federal Toxics Release Inventory (TRI) for 2017–2019."), cite("7"),
        run(" The Colorado Department of Public Health and Environment (CDPHE) identified three such facilities in the North Denver and Commerce City (NDCC) area: the Suncor oil and natural gas refinery, the Phillips 66 Denver Terminal, and the Holly Energy Partners (Sinclair) Denver Products Terminal.")]),
  "intro HB21-1189")

replace_para("Since March 2023, CDPHE has deployed several high time resolution",
  para([run("Since March 2023, CDPHE has operated mobile laboratories carrying high-time-resolution instruments around these facilities."), cite("8"),
        run(" These instruments resolve hundreds of ambient species in real time,"), cite("9–11"),
        run(" but the program exists primarily to track the three covered air toxics, and we focus on them: benzene, "), h2s(), run(", and HCN. We additionally analyze toluene, xylene, and trimethylbenzene, which the same instruments measure and which help discriminate source types (aromatics of petroleum refining, fuel storage, and combustion; "), h2s(), run(" of wastewater treatment and refinery sulfur processing; HCN of fluid catalytic cracking). All six are measurable at 1-s resolution and sub-ppb to ppb detection limits, enabling the event-scale analysis on which this study rests. Benzene is the only one of the six with an inhalation unit risk in EPA's Integrated Risk Information System (IRIS) and is therefore the only species for which we estimate cancer risk. EPA reports that information is inadequate to assess the carcinogenic potential of hydrogen sulfide and hydrogen cyanide,"), cite("43,44"),
        run(" and no inhalation unit risks have been derived for toluene, xylene, or trimethylbenzene; the absence of cancer-risk estimates for these species therefore reflects missing toxicological information rather than evidence of no carcinogenic hazard. Combining these data with continuous measurements at the nearby La Casa stationary site (Section 2.2) offers a route to characterizing air toxics in space and time that improves on modeled estimates and can inform emissions inventories.")]),
  "intro CDPHE mobile")

replace_para("Other states have also launched similar mobile monitoring",
  para([run("Similar initiatives exist elsewhere: Harris County, Texas monitors air toxics near concrete plants,"), cite("12"),
        run(" the California Air Resources Board"), cite("13"),
        run(" and South Coast Air Quality Management District"), cite("14"),
        run(" operate mobile laboratories, and Washington State is developing a program."), cite("15"),
        run(" All aim to collect high-resolution data in communities near polluting facilities, to support exposure assessment, epidemiology, and emissions estimation.")]),
  "intro other states")

# niche paragraph (new) before the contributions paragraph
insert_before("In this study, we leveraged data collected using the CDPHE mobile",
  para([run("Mobile monitoring occupies a distinct niche. Fixed regulatory monitors provide continuous, reference-grade data at very few locations; low-cost sensor networks add spatial density but sacrifice accuracy and species coverage, and no deployed sensor measures HCN or "), h2s(), run(" at ambient levels. Mobile platforms carrying research-grade instruments close this gap: repeated-drive designs have mapped street-level pollutant patterns at 30-100 m scales and localized industrial sources."), cite("37–39"),
        run(" The approach has recently been extended from traffic-related pollutants to hazardous air pollutants at industrial fencelines: mobile campaigns in southeastern Pennsylvania and in the Baton Rouge–New Orleans petrochemical corridor have quantified dozens of HAPs at 1-s resolution and aggregated them to 500 m grid cells and census tracts for exposure and risk estimation."), cite("40–42"),
        run(" Persistent challenges are temporal representativeness (each location is sampled only briefly, usually during working hours), separating local enhancements from background, inlet and instrument response delays at driving speed, and the lack of standard methods for converting transient samples into long-term exposure estimates. This study addresses each explicitly.")]))
print("  [inserted] mobile-monitoring niche paragraph")

replace_para("In this study, we leveraged data collected using the CDPHE mobile",
  para([run("To our knowledge this is the first peer-reviewed analysis of the HB21-1189 monitoring program, covering February 2023-June 2025, and we build an end-to-end reproducible pipeline running from the raw public data to the metrics reported here. Specifically, we (1) map spatially resolved pollutant mixing ratios; (2) compare census block-level concentrations, and the benzene cancer risks derived from them, with EPA's 2020 AirToxScreen, showing that a screening model can assign comparable aggregate risk to substantially different communities, with direct implications for environmental-justice screening; (3) identify persistent hotspots of pollution, and show that co-measured methane distinguishes gas-associated hotspots from combustion- and evaporative-type ones; and (4) extend methods that combine stationary and mobile observations"), cite("16–18"),
        run(" to estimate facility emission rates by inverse Gaussian plume modeling, with a simulation framework that evaluates sensitivity to key assumptions and quantifies the smallest emission rate the method can detect.")]),
  "contributions")

# ================================================================
# 2. Section 2.1: study-domain paragraph; CAT-EMU comparability;
#    delay + native-cadence paragraphs; QA/QC tail trim + follow-on.
# ================================================================
insert_after("2.1.1 Mobile Monitoring Platform",
  para([run("The study domain spans approximately 30 km (east-west) by 20 km (north-south) across north Denver, Commerce City, and adjacent Adams County, Colorado, centered on the industrial corridor along the South Platte River and Sand Creek. The corridor contains the Suncor Energy refinery (Colorado's only petroleum refinery), the Sinclair and Phillips 66 petroleum products terminals, two wastewater treatment facilities, and 752 Toxics Release Inventory facilities, interlaced with major highways (I-70, I-270, I-76, I-25, and US-85/Vasquez Boulevard). The surrounding neighborhoods - including Globeville, Elyria-Swansea, and central Commerce City - are among Colorado's most environmentally burdened communities. The census blocks common to our measurements and AirToxScreen comprise 1,668 blocks housing 126,607 residents (Section 2.5). Two fixed routes covered this domain (Figure 1).")]))
print("  [inserted] study-domain paragraph")

insert_after("A Gill MaxiMet GMX500 weather station",
  para([run("On the two occasions when both laboratories sampled the same 500 m grid cells on the same day (29 cell-days), median EMU/CAT ratios of within-cell daily medians were 1.00 for benzene and 0.99 for toluene, with larger divergence for trimethylbenzene (0.70) and xylene (0.75); "), h2s(), run(" and HCN comparisons are dominated by below-MDL values (SI Section S1.5, Figure S1.1).")]))
print("  [inserted] CAT-EMU comparability")

insert_after("More information about the instruments and QA/QC processes is provided in ",
  para([run("Because sampled air must travel through the inlet lines before reaching each instrument, and instrument response times differ, we applied instrument- and vehicle-specific time-delay corrections to align every measurement with the GPS position at which the sampled air entered the inlet. Delays measured by CDPHE were 4 s for the aromatics, 6 s for HCN, and 21 s for "), h2s(), run(" and CH"), run("4", sub=True), run(" on the CAT, and 5 s for the aromatics, 3 s for HCN, and 17 s for "), h2s(), run(" and CH"), run("4", sub=True), run(" on the EMU. All analyses in this study use the delay-corrected data (more details in section S1 in the SI).")]) +
  para([run("CDPHE delivers all channels on a common one-second time base. The Vocus Eiger acquires once per second, so its aromatic mixing ratios are genuine 1 Hz measurements; the Vocus B (HCN) and Picarro G2204 ("), h2s(), run(" and CH"), run("4", sub=True), run(") acquire every two and five seconds, respectively, and in the delivered data their most recent reading is carried forward to each intervening second. Because treating these carried-forward values as independent one-second observations would overstate the information content of the two slower channels, we averaged "), h2s(), run(", CH"), run("4", sub=True), run(" and HCN, after the delay correction, to their native acquisition cadence (five seconds for "), h2s(), run(" and CH"), run("4", sub=True), run(", both measured on the Picarro G2204, and two seconds for HCN), forming each block mean only from seconds that carried a measured value and applying no gap-filling or interpolation to concentrations. The aromatics were retained at their native one-second resolution. For the emission-rate analysis (Section 2.5.5), where the sub-five-second rise and fall of an "), h2s(), run(" transient carries the shape information the plume inversion depends on, plumes were detected on the delivered (un-averaged) "), h2s(), run(" signal; native-cadence averaging is applied only to the mapping and hotspot analyses. Where several pollutants were compared at the same points, all species were placed on a common five-second cadence (the coarsest native interval) so that no channel was over-represented. Vehicle position and meteorology were linearly interpolated across gaps of up to three seconds; pollutant concentrations were never interpolated.")]))
print("  [inserted] delay + native-cadence paragraphs")

# trim the now-duplicated tail inside the QA/QC paragraph (spans H2S subscript
# runs, so splice at text level: both cut points sit inside red w:t elements)
_i1 = d.find(esc("Before any analysis, each pollutant was shifted back"))
_m2 = esc("so that no gap is filled.")
_j = d.find(_m2, _i1) + len(_m2)
assert _i1 > 0 and _j > _i1, "QA/QC tail splice anchors not found"
d = d[:_i1] + esc("The delay correction and native-cadence averaging applied before any analysis are described above.") + d[_j:]
print("  [spliced] QA/QC tail trim")

insert_after("including the one place where CDPHE's read-me documents and the delivered files disagree.",
  para([run("Negative readings arise because the instruments report the measured signal even when noise carries it below zero; retaining them keeps low-concentration medians unbiased, whereas censoring them would inflate aggregate statistics (SI Section S3 quantifies the alternatives). "), h2s(), run(" and HCN are reported at integer-ppb resolution, so below-MDL values are strongly quantized (the median delivered "), h2s(), run(" value is 0 ppb; the native-cadence averaging described above partially de-quantizes the analysed series). After these exclusions the HCN record comprises 525,467 one-second observations (481,471 native-cadence averages) over 39 sampling days from 22 January 2025 onward. Monthly per-vehicle medians are stable across the 29-month record, arguing against instrument drift (SI Section S1.6, Figure S1.2). Flag-based counts and the resulting fraction of measurements below the MDL for each pollutant are summarized in Table S3.1.")]))
print("  [inserted] qualifier follow-on paragraph")

# ================================================================
# 3. Section 2.4: measurement-based AirToxScreen literature sentence
# ================================================================
d = redfix.red_replace(d,
  "Several studies conclude that AirToxScreen underpredicts concentrations (and hence health risks) for the majority of air toxics modeled.",
  "Several studies conclude that AirToxScreen underpredicts concentrations (and hence health risks) for the majority of air toxics modeled. Recent mobile-monitoring campaigns in industrial corridors reach the same conclusion from direct measurement, finding measured concentrations, and the cancer risks derived from them, above AirToxScreen for most census tracts sampled.",
  "2.4 measurement literature")
# superscript the new citation
i = d.find(esc("above AirToxScreen for most census tracts sampled."))
j = d.find("</w:t></w:r>", i) + len("</w:t></w:r>")
d = d[:j] + cite("40,41") + d[j:]
print("  [added] refs 40,41 cite in 2.4")

# ================================================================
# 4. Section 2.5.3: threshold paragraph fix (resolution wording + 4.8 + typo)
# ================================================================
d = redfix.red_replace(d,
  "we evaluted raw 1 s mixing ratios across the full study period, and classified measurements exceeding the pollutant-specific 99th percentile (1.8 ppb for benzene; 4.31 ppb for toluene; 2.59 for trimethylbenzene; 3.19 ppb for xylene; ",
  "we evaluated mixing ratios across the full study period at the resolution described in Section 2.1 (1 s for the aromatics; native-cadence averages for H2S and HCN), and classified measurements exceeding the pollutant-specific 99th percentile (1.8 ppb for benzene; 4.31 ppb for toluene; 2.59 ppb for trimethylbenzene; 3.19 ppb for xylene; ",
  "threshold paragraph")

# funnel follow-on after the DBSCAN paragraph
insert_after("retaining 35, 33, 32, 35, 58 and 23 clusters respectively.",
  para([run("These 216 persistent single-pollutant clusters were then spatially grouped across pollutants into 151 candidate hotspot groups. Of these, 37 contained at least two pollutants, 17 contained three or more, and 8 contained four or more. Persistent multi-pollutant hotspots were defined as groups exhibiting exceedances for at least three pollutants, resulting in 17 final hotspot groups; the maximum number of co-occurring persistent pollutants within a single group was five. These groups represent locations repeatedly impacted by elevated concentrations, rather than continuous emissions. We emphasize that the event thresholds are statistical reference points - pollutant-specific 99th percentiles chosen to identify relative extremes for source detection - and are not health-based benchmarks; a comparison of campaign concentrations against health-based reference values, together with hazard quotients for the species with non-cancer reference concentrations, is provided in SI Table S3.2 and SI Section S7.")]))
print("  [inserted] funnel + health-benchmark paragraph")

# directional paragraph: add heatmap + sensitivity sentences
d = redfix.red_replace(d,
  "Enrichment ratios greater than 1 indicate that elevated observations occurred more frequently than expected when the candidate source was upwind.",
  "Enrichment ratios greater than 1 indicate that elevated observations occurred more frequently than expected when the candidate source was upwind. Results of this analysis are presented as directional-enrichment heatmaps in Figures S5.4 and S5.5. A 27-variant factorial sensitivity analysis perturbing the event threshold (p98.5-p99.5), clustering radius (50-200 m), and persistence percentile (p85-p95) shows that the number of groups scales with screening stringency, as expected, but that the locations of the major multi-pollutant hotspots are robust: 41-94% of the 17 baseline group locations are recovered within 300 m under single-step perturbations of the threshold, radius, or persistence criterion, falling to 24% only when the clustering radius is doubled (SI Section S5.3, Figure S5.6).",
  "directional sensitivity")

# ================================================================
# 5. Section 2.5.5: two-tier attribution methods
# ================================================================
insert_after("Emission rates are reported in metric tons/yr",
  para([run("Because the wastewater treatment facility is not the only local source of "), h2s(), run(" — the Suncor refinery to the north also emits "), h2s(), run(" from sulfur recovery, together with aromatics and hydrogen cyanide — we did not attribute the retained plumes to the wastewater facility on wind geometry alone. We applied a two-tier chemical test. First, a fingerprint gate exploited the contrast in co-emitted species: wastewater "), h2s(), run(" (anaerobic digester and sewer gas) is essentially free of aromatics and HCN, whereas refinery "), h2s(), run(" co-occurs with BTEX aromatics (from tankage and flares) and HCN (from the fluid catalytic cracking unit). Using the same three-standard-deviation rolling-background criterion applied to "), h2s(), run(", we flagged each retained "), h2s(), run(" plume for any co-located enhancement of benzene, toluene, xylene, trimethylbenzene, or HCN; plumes with no aromatic or HCN co-enhancement were treated as wastewater-consistent, and those coinciding with a BTEX or HCN plume were treated as refinery-influenced and excluded from wastewater attribution. Second, as an independent corroboration we tested for co-located methane enhancement, since wastewater digester gas is methane-rich; methane, recorded on the same Picarro analyzer as "), h2s(), run(", was background-corrected identically and its enhancement evaluated at each retained plume. This procedure is implemented as a reproducible add-on to the plume pipeline (SI Section S6.8).")]))
print("  [inserted] two-tier attribution methods")

# ================================================================
# 6. Section 3.3: magnitude expansion + EJ paragraph (replaces the
#    condensed sentences and the earlier S7 pointer sentence)
# ================================================================
d = redfix.red_replace(d,
  "Mobile-derived benzene is lower than AirToxScreen in 77% of the 1,668 shared blocks (median ratio 0.77), higher by a factor of two or more in 6% of blocks, and higher by an order of magnitude in three blocks. The disagreement between the two datasets is therefore one of spatial pattern rather than of overall level",
  "The magnitude of this spatial disagreement is substantial. AirToxScreen block concentrations span only 0.124-0.309 ppb across the entire domain - a 2.5-fold range - whereas the scaled mobile concentrations span more than an order of magnitude, reaching 2.3 ppb. In 98 of the 1,668 common blocks (5.9%), mobile-derived concentrations exceed AirToxScreen by more than a factor of two, in 18 blocks by more than a factor of five, and in the most extreme block by a factor of 18; none of these localized elevations can be represented in the screening model, whose maximum block value is 0.309 ppb. Conversely, mobile-derived benzene is lower than AirToxScreen in 77% of blocks (median ratio 0.77), because block medians in residential areas away from the industrial corridor fall below the smooth modeled surface. We note that this comparison uses the median of daily medians, a deliberately robust metric that suppresses episodic plume contributions; metrics weighted toward the upper tail of the concentration distribution would show mobile-derived exposures exceeding AirToxScreen overall. The aggregate agreement thus reflects offsetting disagreements: the screening model captures the regional total while missing the specific blocks in which exposure is concentrated. The disagreement between the two datasets is therefore one of spatial pattern rather than of overall level",
  "3.3 magnitude expansion")

# Robinson contrast sentence replaces the pointer-run (pointer re-added inside EJ para)
d = redfix.red_replace(d,
  "shows no correlation between them. Beyond cancer risk, a screening cumulative noncancer hazard assessment of all six measured pollutants (SI Section S7) finds organ-system hazard indices below 1 for neurological and hematological effects but at or above 1 for endocrine effects (hydrogen cyanide) and, in the most-exposed blocks, respiratory effects (hydrogen sulfide), underscoring that a single-pollutant, single-endpoint view can understate the cumulative burden in this community.",
  "shows no correlation between them. This pattern differs from the closest published benchmark of AirToxScreen against mobile measurements, in the Baton Rouge-New Orleans petrochemical corridor, where measurement-based total cancer risk exceeded the model in 14 of 15 census tracts by a median factor of five. Three differences plausibly reconcile the two results: that comparison summed 17 carcinogens and was dominated by ethylene oxide, a species for which AirToxScreen assigns essentially no regional background, whereas benzene is comparatively well represented in the National Emissions Inventory; it was made at census-tract rather than census-block resolution, which averages over the sub-kilometer gradients that dominate our domain; and it did not use a median-of-daily-medians metric, which by construction suppresses the episodic plumes that drive the upper tail. Our result is therefore not that screening models agree with measurements in general, but that for a single, comparatively well-inventoried species the aggregate burden can agree while its spatial allocation does not.",
  "3.3 Robinson contrast")
i = d.find(esc("exceeded the model in 14 of 15 census tracts by a median factor of five."))
j = d.find("</w:t></w:r>", i) + len("</w:t></w:r>")
d = d[:j] + cite("41") + d[j:]
print("  [added] ref 41 cite in 3.3")

insert_after("the aggregate burden can agree while its spatial allocation does not.",
  para([run("We further asked where these under-represented exposures fall relative to Colorado's environmental-justice designations, joining every analyzed block to Colorado EnviroScreen v2 block-group scores (all 1,668 blocks matched). The result is twofold. First, the study domain as a whole is among the most burdened in the state: the median analyzed block sits at the 86th percentile of the statewide EnviroScreen distribution, and 81% of the analyzed blocks - home to the great majority of the 126,607 residents - lie in block groups formally designated as disproportionately impacted (DI) communities. Second, within this already-burdened domain the blocks where mobile-derived concentrations exceed AirToxScreen by more than a factor of two are not systematically the more burdened ones (median EnviroScreen percentile 84.8 vs 86.4 elsewhere, Wilcoxon p = 0.96; 80% vs 81% in DI communities). The environmental-justice implication therefore lies not in a gradient within the domain but in the aggregate: essentially all of the localized benzene exposure that the screening model fails to represent - affecting 7,420 residents in the 98 most extreme blocks alone - accrues to communities the state has already identified as disproportionately impacted, and would be invisible to a monitoring or modeling strategy that relied on AirToxScreen alone (SI Section S4.6). Beyond cancer risk, a screening cumulative noncancer hazard assessment of all six measured pollutants (SI Section S7) finds organ-system hazard indices below 1 for neurological and hematological effects but at or above 1 for endocrine effects (hydrogen cyanide) and, in the most-exposed blocks, respiratory effects (hydrogen sulfide), underscoring that a single-pollutant, single-endpoint view can understate the cumulative burden in this community.")]))
print("  [inserted] EnviroScreen paragraph")

# ================================================================
# 7. Section 3.4.2: composition specifics; Group 9 additions
# ================================================================
_i1 = d.find(esc("Most groups included benzene together with one or more aromatic VOCs"))
_m2 = esc("S and/or HCN.")
_j = d.find(_m2, _i1) + len(_m2)
assert _i1 > 0 and _j > _i1, "composition splice anchors not found"
d = (d[:_i1] + '</w:t></w:r>' +
     run("Benzene was present in 13 of the 17 groups, in most cases together with one or more of the other aromatics (toluene, xylene, or trimethylbenzene); six groups also included a reduced sulfur or nitrogen species, with ") + h2s() +
     run(" in five and HCN in two. No group contained all six pollutants, and the largest mixtures comprised five (Groups 9, 12 and 34).") +
     '<w:r><w:rPr><w:rtl w:val="0"/></w:rPr><w:t xml:space="preserve">' + d[_j:])
print("  [spliced] composition specifics")

d = redfix.red_replace(d,
  "Group 9 is the most heavily sampled hotspot in the study, ",
  "Group 9, approximately 1 km from a gas-fired glass-container plant in Wheat Ridge, is the most heavily sampled hotspot in the study, ",
  "Group 9 glass plant")
d = redfix.red_replace(d,
  "with 37,669 mobile measurements within 100 m and exceedances on 42 distinct days, and it is also the least directionally specific.",
  "with 37,669 mobile measurements within 100 m and exceedances on 42 distinct days, and it is also the least directionally specific: no industrial-corridor facility shows a directional enrichment above 1.0 there. It carries the most extreme trimethylbenzene/benzene ratio of any group (~3.2; Figure 4D) and by far the strongest methane co-elevation (36.8% of nearby observations at or above the campaign 95th percentile, on 11 sampling days; Section 3.7), consistent with a gas-fired combustion source; it is one of three five-pollutant groups and the only one on the western side of the domain.",
  "Group 9 expansion")

# ================================================================
# 8. Section 3.6: attribution results paragraph
# ================================================================
insert_after("do not allow unambiguous attribution of these plumes to a single facility",
  para([run("To narrow this attribution, we tested the chemical composition of the four retained plumes (Section 2.5.5). None showed a co-located enhancement of any aromatic or of hydrogen cyanide at the same three-standard-deviation threshold used for "), h2s(), run(" — the aromatic- and HCN-free signature expected for wastewater (anaerobic digester and sewer) gas, and distinct from refinery emissions, which co-emit BTEX aromatics and HCN. Co-located methane, evaluated as an independent tracer wherever the channel was recording, showed only small enhancements at the plume points (≤0.09 ppm above a background near 2.0 ppm) that did not reach our corroboration threshold; because methane here is uncalibrated and interpreted only in relative terms (Section 3.7), the plumes were intercepted 2.0–4.3 km downwind, and hydrogen sulfide can be generated in liquids- and solids-handling operations separate from the methane-capturing digesters, this neither confirms nor contradicts a wastewater origin. On balance, wind arriving from the wastewater facility together with an aromatic- and HCN-free "), h2s(), run(" signature points to the wastewater facility rather than the refinery as the most likely origin of these plumes (SI Section S6.8, Table S6.2), while the inversion itself cannot exclude minor contributions from other nearby sources.")]))
print("  [inserted] attribution results")

# ================================================================
# 9. Section 3.7 Methane (new) + renumber Limitations to 3.8
# ================================================================
methane = (
  para([run("3.7 Methane")], style="Heading2") +
  para([run("In addition to the regulated air toxics, the Picarro G2204 instruments on both mobile laboratories recorded methane (CH"), run("4", sub=True), run("). We applied the same delay corrections, background correction, wind assignment, and hotspot algorithms described above to 1,837,632 1-s methane observations collected on 193 sampling days (methane data acquisition began after the start of the air-toxics record). High-methane events (≥ 99th percentile, 2.57 ppm) grouped into 66 spatial clusters, of which two met the same persistence criteria used for the air toxics. The largest persistent methane hotspot (39.823° N, 104.974° W; events on 66 of 193 days, maximum 19.3 ppm) lies within the refinery-adjacent industrial corridor, and the second (39.782° N, 104.986° W; 57 days) lies along the Vasquez Boulevard corridor. Both are more than 2.6 km from the nearest persistent multi-pollutant air-toxics hotspot group, indicating sources distinct from those driving the air-toxics hotspots. A source-probability analysis of high-methane events, analogous to Figure 3, places the most likely upwind source region to the northeast of the sampling domain (maximum at 39.872° N, 104.876° W; SI Figure S8.1).")]) +
  para([run("Methane also sharpens the source attribution of the air-toxics hotspot groups themselves. Within 100 m of each of the 17 persistent hotspot groups, the fraction of methane observations at or above the campaign-wide 95th percentile ranged from 0.2% to 36.8%. Methane co-elevation was by far strongest at Group 9, near the western end of the domain (36.8% of observations, with high-methane events on 11 sampling days), a group whose trimethylbenzene-rich ratio signature is characteristic of petroleum/industrial sources, whereas 10 of the 17 groups, including several heavily sampled groups in the industrial corridor, showed less than 5% methane co-elevation, consistent with traffic- or solvent-driven aromatics. Methane thus provides an independent tracer that separates gas-associated hotspots from combustion and evaporative hotspots (SI Figure S8.2).")]) +
  para([run("Because the methane channel was not routinely calibrated during the campaign (a single field check recovered 96.4–97.8% of a 5 ppm standard), we interpret methane concentrations in relative terms and use them primarily for spatial source identification rather than absolute quantification (more details in section S8 in the SI).")]))
insert_before("3.7 Limitations", methane)
print("  [inserted] Section 3.7 Methane")

d = redfix.red_replace(d, "3.7 Limitations", "3.8 Limitations", "renumber Limitations")

# ================================================================
# 10. Limitations: substitution-analysis sentence + risk-narrowness para
# ================================================================
d = redfix.red_replace(d,
  "This focus prioritizes detection of localized emission impacts but may underrepresent broader low-level exposure patterns.",
  "This focus prioritizes detection of localized emission impacts but may underrepresent broader low-level exposure patterns. A formal substitution analysis (SI Section S3, Figure S3.11) shows that replacing below-MDL values with MDL/2 or the MDL would degrade rather than improve the spatial information used in the census-block comparison - the correlation of the block-level benzene surface with the raw-value surface falls to 0.35 and 0.01, respectively, while inflating block concentrations to several times AirToxScreen levels - supporting our use of raw reported values with a focus on high-concentration events.",
  "substitution analysis sentence")

insert_after("its spatial and seasonal patterns should be read accordingly.",
  para([run("Our risk estimates are also deliberately narrow. Of the six species analyzed, only benzene has an inhalation unit risk in EPA's IRIS, so cancer risk is estimated for benzene alone; for "), h2s(), run(" and HCN, EPA states that the available information is inadequate to assess carcinogenic potential,"), cite("43,44"),
        run(" and no unit risks exist for toluene, xylene, or trimethylbenzene. On the noncancer side we provide a screening cumulative hazard assessment by target organ system (SI Section S7); recent work shows that building cumulative hazard on all known target-organ systems, rather than on the single most sensitive endpoint per chemical, brings substantially more measured species into the calculation and raises estimated hazard,"), cite("42"),
        run(" so even that screening likely understates the total burden. The risk figures reported here should therefore be read as a bound restricted to well-characterized endpoints, not as a full characterization of the health burden in the study domain.")]))
print("  [inserted] risk-narrowness limitation")

# ================================================================
# 11. Discussion: permit-record specifics
# ================================================================
d = redfix.red_replace(d,
  "and, despite uncertainty, were large relative to reported emissions for individual facilities in the corridor.",
  "and, despite uncertainty, were large relative to the quantities that appear in the facility permit record - at a comparable wastewater facility operated by the same district, reported H2S from permitted combustion sources totals roughly 0.2 metric tons/yr, and the wastewater treatment process itself is exempt from permitting, so no reported H2S value exists for the liquid- and solids-handling operations that generate it (SI Section S6.7).",
  "permit-record specifics")

# ================================================================
# 12. Data and Code Availability (before References)
# ================================================================
dca = (para([run("Data and Code Availability")], style="Heading1") +
  para([run("All mobile air-toxics data used in this study are publicly available from the CDPHE air toxics data repository (https://www.colorado.gov/airquality/air_toxics_repo.aspx); the local copies used here were verified to match the posted quarterly data packets for the study period. Methane data were obtained directly from CDPHE. All analysis code is available at https://github.com/pdez90/AirToxics_Colorado. The repository includes a single-command pipeline (RUN_ALL_from_raw.R) that reproduces every number, figure, and table in this manuscript and its Supporting Information from the raw CDPHE data, including input-verification and diagnostic checks at each stage; all results reported here were confirmed by repeated end-to-end runs of this pipeline from a clean state.")]))
i = d.find(">References<")
ps = d.rfind("<w:p ", 0, i)
d = d[:ps] + dca + d[ps:]
print("  [inserted] Data and Code Availability")

# ================================================================
# 13. References 37-44 (ACS style, "(N)" + tab, red)
# ================================================================
def refpara(num, text):
    ppr = ('<w:pPr><w:keepNext w:val="0"/><w:keepLines w:val="0"/>'
           '<w:pageBreakBefore w:val="0"/><w:widowControl w:val="0"/>'
           '<w:spacing w:after="0" w:before="0" w:line="240" w:lineRule="auto"/>'
           '<w:ind w:left="384" w:right="0" w:hanging="384"/>'
           '<w:jc w:val="left"/><w:rPr/></w:pPr>')
    r1 = run("(%d)" % num)
    tab = '<w:r><w:rPr>%s<w:rtl w:val="0"/></w:rPr><w:tab/></w:r>' % RED
    return "<w:p>%s%s%s%s</w:p>" % (ppr, r1, tab, run(text))

REFS = [
 (37, "Apte, J. S.; Messier, K. P.; Gani, S.; Brauer, M.; Kirchstetter, T. W.; Lunden, M. M.; Marshall, J. D.; Portier, C. J.; Vermeulen, R. C. H.; Hamburg, S. P. High-Resolution Air Pollution Mapping with Google Street View Cars: Exploiting Big Data. Environ. Sci. Technol. 2017, 51 (12), 6999-7008."),
 (38, "Messier, K. P.; Chambliss, S. E.; Gani, S.; Alvarez, R.; Brauer, M.; Choi, J. J.; Hamburg, S. P.; Kerckhoffs, J.; LaFranchi, B.; Lunden, M. M.; Marshall, J. D.; Portier, C. J.; Roy, A.; Szpiro, A. A.; Vermeulen, R. C. H.; Apte, J. S. Mapping Air Pollution with Google Street View Cars: Efficient Approaches with Mobile Monitoring and Land Use Regression. Environ. Sci. Technol. 2018, 52 (21), 12563-12572."),
 (39, "Chambliss, S. E.; Preble, C. V.; Caubel, J. J.; Cados, T.; Messier, K. P.; Alvarez, R. A.; LaFranchi, B.; Lunden, M.; Marshall, J. D.; Szpiro, A. A.; Kirchstetter, T. W.; Apte, J. S. Comparison of Mobile and Fixed-Site Black Carbon Measurements for High-Resolution Urban Pollution Mapping. Environ. Res. Lett. 2020, 15, 084041."),
 (40, "Robinson, E. S.; Tehrani, M. W.; Yassine, A.; et al. Ethylene Oxide in Southeastern Louisiana's Petrochemical Corridor: High Spatial Resolution Mobile Monitoring during HAP-MAP. Environ. Sci. Technol. 2024, 58 (25), 11084-11095. https://doi.org/10.1021/acs.est.3c10579."),
 (41, "Robinson, E. S.; Yassine, A.; Agarwal, S.; et al. Total Cancer Risk Estimates from Measured Concentrations of Volatile Organic Compounds in Industrialized Southeastern Louisiana. Proc. Natl. Acad. Sci. U.S.A. 2025, 122 (41), e2504770122. https://doi.org/10.1073/pnas.2504770122."),
 (42, "Chiger, A. A.; Gigot, C.; Robinson, E. S.; et al. Improving Methodologies for Cumulative Risk Assessment: A Case Study of Noncarcinogenic Health Risks from Volatile Organic Compounds in Fenceline Communities in Southeastern Pennsylvania. Environ. Health Perspect. 2025, 133 (5), 057004. https://doi.org/10.1289/EHP14696."),
 (43, "US EPA. Integrated Risk Information System: Hydrogen Sulfide (CASRN 7783-06-4); the database states that data are inadequate for an assessment of the carcinogenic potential. https://iris.epa.gov (accessed August 2026)."),
 (44, "US EPA. Integrated Risk Information System: Hydrogen Cyanide (CASRN 74-90-8); the database states that there is inadequate information to assess the carcinogenic potential. https://iris.epa.gov (accessed August 2026)."),
]
i = d.find("(36)")
pe = d.find("</w:p>", i) + len("</w:p>")
d = d[:pe] + "".join(refpara(n, t) for n, t in REFS) + d[pe:]
print("  [appended] references 37-44")

open(P, "w", encoding="utf-8").write(d)
print("document.xml rewritten (%d bytes)" % len(d))
