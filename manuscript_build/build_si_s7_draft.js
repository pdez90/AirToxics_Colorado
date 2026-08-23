const docx = require("docx");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, BorderStyle, ShadingType, PageOrientation
} = docx;
const fs = require("fs");

const RED = "FF0000";
const FONT = "Times New Roman";
const SZ = 22; // 11pt

function t(text, opts = {}) {
  return new TextRun({ text, font: FONT, size: opts.sz || SZ,
    bold: !!opts.bold, italics: !!opts.italics, color: opts.color,
    superScript: opts.sup, subScript: opts.sub });
}
// prose paragraph from an array of TextRuns
function para(runs, opts = {}) {
  return new Paragraph({ children: runs, spacing: { after: opts.after ?? 160, line: 276 },
    alignment: opts.align, indent: opts.indent });
}
// convenience: single-color prose from a plain string with optional inline segments
function p(str, opts = {}) { return para([t(str, opts)], opts); }

function h(text, level) {
  return new Paragraph({ heading: level,
    children: [new TextRun({ text, font: FONT, bold: true,
      size: level === HeadingLevel.HEADING_1 ? 26 : 24, color: "000000" })],
    spacing: { before: 240, after: 120 } });
}

// ---- table helpers ----
function cell(children, opts = {}) {
  const kids = Array.isArray(children) ? children : [children];
  return new TableCell({
    width: { size: opts.w, type: WidthType.DXA },
    shading: opts.shade ? { type: ShadingType.CLEAR, fill: opts.shade, color: "auto" } : undefined,
    margins: { top: 40, bottom: 40, left: 80, right: 80 },
    verticalAlign: docx.VerticalAlign.CENTER,
    children: kids.map(c => (typeof c === "string")
      ? new Paragraph({ children: [t(c, { bold: opts.bold, sz: 20, color: opts.color })],
          alignment: opts.align || AlignmentType.LEFT, spacing: { after: 0, line: 240 } })
      : c)
  });
}
function hrow(labels, widths, shade) {
  return new TableRow({ tableHeader: true,
    children: labels.map((l, i) => cell(l, { w: widths[i], bold: true, shade: shade || "D9D9D9",
      align: i === 0 ? AlignmentType.LEFT : AlignmentType.CENTER })) });
}
function drow(vals, widths, opts = {}) {
  return new TableRow({ children: vals.map((v, i) =>
    cell(String(v), { w: widths[i], align: i === 0 ? AlignmentType.LEFT : AlignmentType.CENTER,
      bold: opts.bold, shade: opts.shade, color: opts.color })) });
}
function mkTable(rows, widths) {
  return new Table({ columnWidths: widths, width: { size: widths.reduce((a,b)=>a+b,0), type: WidthType.DXA },
    borders: {
      top:{style:BorderStyle.SINGLE,size:4,color:"888888"}, bottom:{style:BorderStyle.SINGLE,size:4,color:"888888"},
      left:{style:BorderStyle.NONE}, right:{style:BorderStyle.NONE},
      insideHorizontal:{style:BorderStyle.SINGLE,size:2,color:"CCCCCC"}, insideVertical:{style:BorderStyle.NONE} },
    rows });
}

const body = [];

// banner note
body.push(new Paragraph({
  border: { top:{style:BorderStyle.SINGLE,size:6,color:RED}, bottom:{style:BorderStyle.SINGLE,size:6,color:RED},
            left:{style:BorderStyle.SINGLE,size:6,color:RED}, right:{style:BorderStyle.SINGLE,size:6,color:RED} },
  shading: { type: ShadingType.CLEAR, fill: "FFF3F3", color: "auto" },
  spacing: { after: 240 },
  children: [ t("DRAFT for author review — proposed new SI Section S7. ", { bold: true }),
    t("On insertion into the SI this entire section is marked in red per the revision convention. Every number is emitted by "),
    t("R_scripts/74_health_hazard_screening.R", { italics: true }),
    t(" and reproduces from the raw CDPHE files through the existing pipeline; nothing here is typed by hand.") ]
}));

// ===== S7 =====
body.push(h("S7  Screening-level cumulative noncancer hazard assessment", HeadingLevel.HEADING_1));

body.push(para([
  t("Section 3.3 of the main text quantifies excess lifetime "),
  t("cancer", { italics: true }),
  t(" risk from benzene. Here we complement that analysis with a screening-level, cumulative "),
  t("noncancer", { italics: true }),
  t(" hazard assessment across all six measured pollutants. We follow the hazard-quotient / hazard-index framework recently applied to fenceline-community mobile-monitoring data by Chiger et al. (2025a), which itself operationalizes U.S. EPA guidance for the cumulative risk assessment of chemical mixtures. This mirrors, on the noncancer side, the measurement-based risk translations of Robinson et al. (2024, 2025), and responds to the observation from community-based work in fenceline neighborhoods that a health assessment restricted to the single most-sensitive endpoint of each chemical can understate the cumulative burden residents actually experience (Chiger et al. 2025b).")
]));

body.push(h("S7.1  Methods", HeadingLevel.HEADING_2));

body.push(para([
  t("For each pollutant "), t("i", { italics: true }),
  t(" we compute a hazard quotient, the ratio of an exposure concentration to the chronic inhalation reference concentration (RfC), and sum the hazard quotients of the pollutants acting on a common target organ system into a hazard index (Chiger et al. 2025a):")
]));
body.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing:{after:80},
  children:[ t("HQ", { italics:true }), t("i",{italics:true,sub:true}), t(" = EC"),
    t("i",{italics:true,sub:true}), t(" / RfC"), t("i",{italics:true,sub:true}), t("       (Eq. S7.1)") ]}));
body.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing:{after:160},
  children:[ t("HI", { italics:true }), t("organ",{italics:true,sub:true}), t(" = Σ"),
    t("i",{italics:true,sub:true}), t(" HQ"), t("i",{italics:true,sub:true}),
    t("       (Eq. S7.2)") ]}));
body.push(para([
  t("where EC"), t("i",{italics:true,sub:true}), t(" is the exposure concentration and RfC"),
  t("i",{italics:true,sub:true}),
  t(" the U.S. EPA Integrated Risk Information System (IRIS) chronic inhalation reference concentration. A hazard quotient or index at or above 1 flags an exposure above the level judged, with uncertainty spanning perhaps an order of magnitude, to be without appreciable risk of that effect over a lifetime. Each pollutant is assigned to the target organ system of its IRIS critical effect (Table S7.1); mixing ratios (ppb) are converted to mass concentrations (µg m")
, t("−3",{sup:true}), t(") at 25 °C and the 830 hPa site pressure (molar volume 29.87 L mol"), t("−1",{sup:true}), t("), the same convention used for the benzene inhalation unit risks in §2.4.")
]));
body.push(para([
  t("We characterize exposure two ways from the block-resolved concentrations used in Section 3.3 (median-of-daily and mean-of-daily block summaries): a "),
  t("population-weighted mean", { bold:true }),
  t(" of the block mean-of-daily-mean concentration, representing a community-average long-term exposure, and the single "),
  t("most-exposed census block", { bold:true }),
  t(" as a worst-case. Consistent with every other pipeline stage, negative and below-MDL values are retained in the block means (see the QA/QC description in Section S1.4); population weighting and time-averaging keep the community metric unbiased. We use the block mean-of-daily-mean concentrations without the stationary-site temporal-bias scaling of Section 3.3, so that all six pollutants are treated identically — the scaling factors were derived only for the three aromatics. Applying the aromatic scaling raises the neurological and hematological hazard indices by less than 0.01 and leaves the endocrine and respiratory indices, which are set by unscaled species (H"),
  t("2",{sub:true}), t("S and HCN), unchanged.")
]));
body.push(para([
  t("As a companion "), t("acute", { italics:true }),
  t(" screen, we compare the campaign 99th-percentile and maximum short-term concentrations (Table S3.1) with the California OEHHA 1-hour acute Reference Exposure Levels (RELs). Because our measurements resolve sub-minute peaks while the REL averaging time is one hour, these acute hazard quotients are deliberately conservative upper bounds rather than estimates of a realized one-hour exposure.")
]));
body.push(para([
  t("We follow the most-sensitive-organ (“traditional”) approach of Chiger et al. (2025a). Those authors also pilot an expanded multi-effects toxicity database that assigns each chemical to "),
  t("every", { italics:true }),
  t(" target organ system with an available toxicity value, and show that doing so can raise hazard indices above 1 where the traditional approach finds no concern. With only six pollutants, each carrying a single IRIS RfC, we retain the traditional assignment and flag this expansion as a limitation (Section S7.3) rather than deriving new toxicity values.")
]));

// ---- Table S7.1 caption + table ----
body.push(new Paragraph({ spacing:{before:120, after:80}, children:[
  t("Table S7.1. ", { bold:true }),
  t("Chronic noncancer hazard quotients and organ-system hazard indices for the six measured pollutants. Reference concentrations are EPA IRIS chronic inhalation RfCs; the population-weighted-mean and most-exposed-block columns are the two exposure metrics defined above. Hazard indices ≥ 1 are shaded.", { sz:20 })
]}));

// widths (DXA) sum ~ 9360 (landscape usable)
{
  const W = [2050, 1500, 1250, 1120, 1120, 1120, 1200];
  const rows = [];
  rows.push(hrow(["Pollutant","Target organ\nsystem","IRIS RfC\n(µg m⁻³)","Pop-wt mean\n(µg m⁻³)","HQ, pop-wt\nmean","Most-exposed\nblock (µg m⁻³)","HQ, most-\nexposed block"], W));
  const data = [
    ["Benzene","Hematological","30","0.461","0.015","7.11","0.237"],
    ["Toluene","Neurological","5000","1.42","0.0003","50.7","0.010"],
    ["Xylenes","Neurological","100","1.21","0.012","32.9","0.329"],
    ["1,2,4-Trimethylbenzene","Neurological","60","1.15","0.019","12.9","0.216"],
    ["H₂S","Respiratory","2","0.748","0.374","9.95","4.97"],
    ["HCN","Endocrine (thyroid)","0.8","1.28","1.60","6.97","8.71"],
  ];
  data.forEach(r => rows.push(drow(r, W)));
  // HI summary rows
  rows.push(new TableRow({ children: [ cell("Hazard index by organ system", {w: W[0]+W[1]+W[2]+W[3], bold:true}),
     cell("HI, pop-wt mean", {w: W[4], bold:true, align:AlignmentType.CENTER}),
     cell("", {w: W[5]}), cell("HI, most-exposed block", {w: W[6], bold:true, align:AlignmentType.CENTER}) ]}));
  const his = [
    ["Endocrine (HCN)","1.60","8.71", true],
    ["Respiratory (H₂S)","0.374","4.97", true],
    ["Neurological (toluene + xylenes + 1,2,4-TMB)","0.031","0.555", false],
    ["Hematological (benzene)","0.015","0.237", false],
  ];
  his.forEach(([lab,pw,mx,flag]) => {
    rows.push(new TableRow({ children: [
      cell(lab, {w: W[0]+W[1]+W[2]+W[3], align:AlignmentType.LEFT}),
      cell(pw, {w: W[4], align:AlignmentType.CENTER, bold:flag, shade: (parseFloat(pw)>=1)?"FBD4D4":undefined}),
      cell("", {w: W[5]}),
      cell(mx, {w: W[6], align:AlignmentType.CENTER, bold:flag, shade: (parseFloat(mx)>=1)?"FBD4D4":undefined}) ]}));
  });
  body.push(mkTable(rows, W));
}

body.push(h("S7.2  Results", HeadingLevel.HEADING_2));
body.push(para([
  t("At the population-weighted-mean exposure, four of the six pollutants give hazard quotients well below 1. The exception is hydrogen cyanide: its low reference concentration (0.8 µg m"),
  t("−3",{sup:true}),
  t(") yields an endocrine (thyroid) hazard index of "),
  t("1.60", { bold:true }),
  t(" at the community average and "),
  t("8.71", { bold:true }),
  t(" in the most-exposed block. Hydrogen sulfide gives a respiratory hazard index of 0.37 at the community average but "),
  t("4.97", { bold:true }),
  t(" in the most-exposed block, so it too exceeds 1 for the most-exposed residents. The neurological hazard index (toluene, xylenes and 1,2,4-trimethylbenzene combined) reaches only 0.031 at the community average and 0.56 at the most-exposed block, and the hematological index for benzene reaches 0.015 and 0.24 respectively; neither approaches 1 anywhere in the domain. This pattern — endocrine and respiratory effects exceeding 1 while neurological, hematological and other systems remain below — is consistent with the fenceline cumulative-risk results of Chiger et al. (2025a), in which endocrine, renal, respiratory and neurological indices exceeded 1 once effects beyond the most-sensitive endpoint were considered.")
]));
body.push(para([
  t("The acute screen (Table S7.2) tells a complementary story. At the 99th-percentile short-term concentration every pollutant sits below its OEHHA 1-hour acute REL (largest HQ 0.17, benzene). Only at the single highest instantaneous peak of the entire campaign do benzene (HQ ≈ 55), H"),
  t("2",{sub:true}),
  t("S (HQ ≈ 9.4) and toluene (HQ ≈ 1.8) exceed the 1-hour REL; the trimethylbenzene peak reaches 40% of its REL (HQ ≈ 0.40). Because those peaks are sub-minute excursions compared against a one-hour guideline, they bound rather than estimate an acute exposure; they nonetheless mark benzene and H"),
  t("2",{sub:true}),
  t("S — and to a lesser degree toluene — as the species whose transient plumes most warrant follow-up with time-resolved acute metrics.")
]));

// ---- Table S7.2 ----
body.push(new Paragraph({ spacing:{before:120, after:80}, children:[
  t("Table S7.2. ", { bold:true }),
  t("Acute screen: campaign 99th-percentile and maximum short-term concentrations (Table S3.1) versus OEHHA 1-hour acute RELs (current REL summary, accessed August 2026; the toluene REL reflects OEHHA’s 2020 revision and the trimethylbenzene REL the 2023 adoption). Ratios at the maximum are conservative upper bounds (sub-minute peak vs 1-hour averaging time).", { sz:20 })
]}));
{
  const W = [2100, 1500, 1350, 1150, 1150, 1150];
  const rows = [];
  rows.push(hrow(["Pollutant","OEHHA acute\nREL (µg m⁻³)","p99 conc.\n(µg m⁻³)","HQ at p99","Max conc.\n(µg m⁻³)","HQ at max"], W));
  const data = [
    ["Benzene","27","4.71","0.174","1485.8","55.0", true],
    ["Toluene","5000","13.30","0.00266","8831.9","1.77", true],
    ["Xylenes","22000","11.34","0.00052","4469.5","0.203", false],
    ["1,2,4-Trimethylbenzene","2400","10.42","0.00434","969.8","0.404", false],
    ["H₂S","42","5.48","0.130","394.4","9.39", true],
    ["HCN","340","9.96","0.0293","65.2","0.192", false],
  ];
  data.forEach(([n,rel,p99,h99,mx,hmx,flag]) => {
    rows.push(new TableRow({ children: [
      cell(n, {w:W[0]}), cell(rel,{w:W[1],align:AlignmentType.CENTER}),
      cell(p99,{w:W[2],align:AlignmentType.CENTER}), cell(h99,{w:W[3],align:AlignmentType.CENTER}),
      cell(mx,{w:W[4],align:AlignmentType.CENTER}),
      cell(hmx,{w:W[5],align:AlignmentType.CENTER, bold:flag, shade:(flag?"FBD4D4":undefined)}) ]}));
  });
  body.push(mkTable(rows, W));
}

body.push(h("S7.3  Interpretation and limitations", HeadingLevel.HEADING_2));
body.push(para([
  t("This is a "), t("screening", { italics:true }),
  t(" assessment, not a formal exposure or risk assessment, and several limitations bound its interpretation. First, the mobile campaign is a repeated but finite set of drive-days weighted toward winter conditions; translating measured concentrations into the lifetime average that a reference concentration presumes carries real uncertainty, a caveat shared by every measurement-based HAP risk study of this kind (Robinson et al. 2024, 2025; Chiger et al. 2025a). Second, the endocrine exceedance rests entirely on hydrogen cyanide, whose reference concentration is the lowest of the six and whose record is the thinnest: HCN passes QA/QC only from 22 January 2025 onward (39 measurement days, three May calibration days excluded) and covers "),
  t("~90,000", { bold:false }),
  t(" of the ~127,000 residents. The thyroid endpoint and the magnitude of the exceedance make HCN a clear priority for targeted follow-up, but the single-species, short-record basis means the endocrine index should be read as a flag, not a settled estimate. Third, the respiratory exceedance for H"),
  t("2",{sub:true}),
  t("S is confined to the most-exposed blocks along the corridor near the wastewater-treatment facility identified in Section 3.4, rather than being community-wide. Fourth, single-measurement precision is limited exactly where the hazard signal sits: audited method detection limits for H₂S (2–6 ppb) and HCN (0.18–13 ppb) exceed the mixing ratios equivalent to their RfCs (1.75 and 0.88 ppb at site pressure); the hazard metrics rest on averages over many readings, unbiased because negative and below-MDL values are retained (S1.4).")
]));
body.push(para([
  t("Finally, the hazard-index framework assumes dose-additivity within an organ system and, in the traditional form used here, counts only each chemical’s most-sensitive effect. Both choices are likely to "),
  t("under", { italics:true }),
  t("state cumulative hazard: benzene also acts on the immune system and hydrogen cyanide has a secondary central-nervous-system effect, and Chiger et al. (2025a) show that incorporating additional target organ systems through a multi-effects toxicity database raises several hazard indices above 1 where a most-sensitive-organ analysis finds none. The choice of reference values matters in the same direction: California OEHHA’s chronic RELs for benzene (3 µg m"),
  t("−3",{sup:true}),
  t(") and the trimethylbenzenes (4 µg m"),
  t("−3",{sup:true}),
  t(") sit 10- and 15-fold below the corresponding IRIS RfCs, and re-anchoring to them would raise the most-exposed-block hazard quotients to 2.37 (benzene, hematological) and 3.24 (1,2,4-trimethylbenzene, neurological) — above 1 — while leaving the community-average picture qualitatively unchanged. The results are also robust to the exposure construction: an independent analysis on the 500 m grid used for the concentration maps (background-corrected concentrations, cells with at least 10 visit-days; script 73_cumulative_risk.R) reproduces the same ordering, with the endocrine index above 1 in 95% of cells at the mean-based metric (maximum 2.89) and the respiratory index above 1 only in the most-exposed cells (maximum 6.19). A full multi-effects treatment for this pollutant suite, together with time-resolved acute metrics, is a natural next step.")
]));

// ---- references block ----
body.push(h("New references cited in this section", HeadingLevel.HEADING_2));
const refs = [
  "Chiger, A. A.; Gigot, C.; Robinson, E. S.; et al. Improving Methodologies for Cumulative Risk Assessment: A Case Study of Noncarcinogenic Health Risks from Volatile Organic Compounds in Fenceline Communities in Southeastern Pennsylvania. Environ. Health Perspect. 2025a, 133 (5), 057004. DOI: 10.1289/EHP14696.",
  "Chiger, A. A.; Alford, E.; Warren, K. N.; et al. Influences of Chemical and Nonchemical Stressors on Health and Quality of Life in Fenceline Communities: A Community-Based Participatory Research Survey in Southeastern Pennsylvania. Environ. Justice 2025b. DOI: 10.1089/env.2024.0078.",
  "Robinson, E. S.; Yassine, A.; Agarwal, S.; et al. Total Cancer Risk Estimates from Measured Concentrations of Volatile Organic Compounds in Industrialized Southeastern Louisiana. Proc. Natl. Acad. Sci. U.S.A. 2025, 122 (41), e2504770122. DOI: 10.1073/pnas.2504770122.",
  "Robinson, E. S.; Tehrani, M. W.; Yassine, A.; et al. Ethylene Oxide in Southeastern Louisiana’s Petrochemical Corridor: High Spatial Resolution Mobile Monitoring during HAP-MAP. Environ. Sci. Technol. 2024, 58, 11084–11095. DOI: 10.1021/acs.est.3c10579.",
  "U.S. EPA. Integrated Risk Information System (IRIS) chronic inhalation reference concentrations: benzene (2003), toluene (2005), xylenes (2003), 1,2,4-trimethylbenzene (2016), hydrogen sulfide (2003), hydrogen cyanide and cyanide salts (2010). https://iris.epa.gov.",
  "OEHHA (California Office of Environmental Health Hazard Assessment). Acute, 8-Hour and Chronic Reference Exposure Level (REL) Summary (accessed August 2026); acute (1-hour) RELs for benzene (2014), toluene (2020 revision), xylenes, trimethylbenzenes (2023), hydrogen sulfide and hydrogen cyanide, and chronic RELs for benzene and trimethylbenzenes. https://oehha.ca.gov/air/general-info/oehha-acute-8-hour-and-chronic-reference-exposure-level-rel-summary.",
];
refs.forEach((r,i) => body.push(new Paragraph({ spacing:{after:100, line:264},
  indent:{ left:360, hanging:360 },
  children:[ t(r, { sz:20 }) ] })));

// ---- optional manuscript hook ----
body.push(h("Optional pointer for the main text (Section 3.3 or Discussion)", HeadingLevel.HEADING_2));
body.push(new Paragraph({ shading:{type:ShadingType.CLEAR, fill:"FFF3F3", color:"auto"},
  border:{ top:{style:BorderStyle.SINGLE,size:4,color:RED}, bottom:{style:BorderStyle.SINGLE,size:4,color:RED},
           left:{style:BorderStyle.SINGLE,size:4,color:RED}, right:{style:BorderStyle.SINGLE,size:4,color:RED} },
  spacing:{after:120},
  children:[ t("Beyond cancer risk, a screening cumulative noncancer assessment of the six pollutants (SI Section S7) finds organ-system hazard indices below 1 for neurological and hematological effects but at or above 1 for endocrine (hydrogen cyanide) and, in the most-exposed blocks, respiratory (hydrogen sulfide) effects, underscoring that a single-pollutant, single-endpoint view understates the cumulative burden in this community.", { color: RED }) ]}));

const doc = new Document({
  styles: { default: { document: { run: { font: FONT, size: SZ } } } },
  sections: [{
    properties: { page: { size: { orientation: PageOrientation.LANDSCAPE, width: 15840, height: 12240 },
      margin: { top: 1000, bottom: 1000, left: 1080, right: 1080 } } },
    children: body
  }]
});

Packer.toBuffer(doc).then(b => { fs.writeFileSync("/tmp/SI_Section_S7_DRAFT.docx", b);
  console.log("wrote SI_Section_S7_DRAFT.docx", b.length, "bytes"); });
