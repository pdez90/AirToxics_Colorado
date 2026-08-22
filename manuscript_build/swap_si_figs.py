"""Swap the 28 numbered SI figures for the 2026-08-21 regenerated versions.

For each figure: rewrite the word/media part in place (same part name, same
format, so no rels/content-type changes are needed), then rewrite the drawing's
<wp:extent>/<a:ext> so the displayed box matches the NEW aspect ratio and fits
inside the 6.5 x 9.0 in text area.

Several figures were laid out 8.5 in wide in the submitted SI, i.e. wider than
the page's text area. They are refitted to 6.5 in here.

The equation graphic (rId109) and the 50 Table S5.1 panels (rId47-rId97) are
NOT touched by this script.
"""
import os, re, shutil, sys
from PIL import Image
Image.MAX_IMAGE_PIXELS = None

UP   = "/tmp/sifig/unpacked"
SRC  = os.path.expanduser("~/mnt/Suncor")
EMU  = 914400
TEXT_W, TEXT_H = int(6.5 * EMU), int(9.0 * EMU)
LONG_SIDE = 2000          # target long-edge resolution for raster parts

# figure -> (rId, media part, source file relative to SRC)
JOBS = [
 ("S3.1",  "rId20",  "word/media/image69.gif", "routes_runs_optimized.gif"),
 ("S3.2",  "rId21",  "word/media/image8.png",  "missingness_by_site.png"),
 ("S3.3",  "rId22",  "word/media/image34.png", "pairs_Suncor_and_Phillips_66_Terminal.png"),
 ("S3.4",  "rId23",  "word/media/image30.png", "pairs_Holly_Energy_Partners_Sinclair_Terminal.png"),
 ("S3.5",  "rId24",  "word/media/image61.jpg", "TimePlot_Suncor.jpeg"),
 ("S3.6",  "rId25",  "word/media/image55.jpg", "TimePlot_Terminal.jpeg"),
 ("S3.7",  "rId26",  "word/media/image52.jpg", "TimeVariation_BTEX_BothRoutes.jpeg"),
 ("S3.8",  "rId27",  "word/media/image51.jpg", "TimeVariation_H2S_HCN_BothRoutes.jpeg"),
 ("S3.9",  "rId28",  "word/media/image60.jpg", "FinalFig/Suncor_PolarPlot_openairmaps.jpeg"),
 ("S3.10", "rId29",  "word/media/image64.jpg", "FinalFig/polarPlot_suncor_wind.jpeg"),
 ("S4.1",  "rId34",  "word/media/image31.png", "FINAL_scaling_figure.png"),
 ("S4.2",  "rId35",  "word/media/image54.png", "FinalFig/tri_inside_outside_1km_distributions.png"),
 ("S4.3",  "rId36",  "word/media/image45.png", "FinalFig/tri_buffer_mean_ci_clean.png"),
 ("S4.4",  "rId37",  "word/media/image53.png", "FinalFig/roadclass_bgcorrected_clean_v3.png"),
 ("S4.5",  "rId38",  "word/media/image14.png", "segment500_ratio_maps/FIG_segment500_ratio_maps_4panel.png"),
 ("S4.6",  "rId39",  "word/media/image27.png", "FinalFig/block_maps_airtox_vs_mobile_scaled_polygons_medofdailymed_ROBUST/FIG_polygons_overlap_AirTox_vs_Mobile_scaled_6panel_ROBUST.png"),
 ("S4.7",  "rId40",  "word/media/image16.png", "FinalFig/block_maps_airtox_vs_mobile_scaled_polygons_medofdailymed_ROBUST/FIG_scatter3_MobileScaled_vs_AirTox_equalPanels_ROBUST.png"),
 ("S4.8",  "rId41",  "word/media/image68.jpg", "traj_low_TMBbyBenz_bottom20pct_gg_paths.jpeg"),
 ("S5.1",  "rId42",  "word/media/image35.png", "sourceprob_diagnostics/B_sensitivity_PANEL_benzene.png"),
 ("S5.2",  "rId43",  "word/media/image11.png", "sourceprob_diagnostics/C_effortnorm_PANEL.png"),
 ("S5.3",  "rId44",  "word/media/image26.png", "sourceprob_diagnostics/D_ratio_PANEL.png"),
 ("S5.4",  "rId45",  "word/media/image38.png", "hotspot_source_fingerprint_outputs/FIG_hotspot_source_frac_high_heatmap.png"),
 ("S5.5",  "rId46",  "word/media/image19.png", "hotspot_source_fingerprint_outputs/FIG_hotspot_source_enrichment_heatmap.png"),
 ("S6.1",  "rId104", "word/media/image13.png", "FIG_WWTP_H2S_plume_funnel.png"),
 ("S6.2",  "rId105", "word/media/image50.png", "FIG_H2S_all_kept_plume_shapes_WWTP.png"),
 ("S6.3",  "rId111", "word/media/image62.png", "error_vs_true_stack_height_WWTP_0p5to5km.png"),
 ("S6.4",  "rId112", "word/media/image10.png", "error_vs_distance_crosswind_mismatch_WWTP.png"),
 ("S6.5",  "rId114", "word/media/image37.png", "FinalFig/FIG_WWTP_baseline_per_plume_timeseries_METRIC_TPY.png"),
]

# ---- pre-flight ----------------------------------------------------------
fail = False
for fig, rid, part, src in JOBS:
    p = os.path.join(SRC, src)
    if not os.path.exists(p):
        print("MISSING SOURCE %-6s %s" % (fig, src)); fail = True
    if not os.path.exists(os.path.join(UP, part)):
        print("MISSING PART   %-6s %s" % (fig, part)); fail = True
if fail:
    sys.exit("pre-flight failed")

xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

# every rId must appear exactly once, and every media part must be referenced
# by exactly one rId (verified separately) -- assert the first here
for fig, rid, part, src in JOBS:
    n = len(re.findall(r'r:embed="%s"' % rid, xml))
    if n != 1:
        sys.exit("rId %s appears %d times (expected 1)" % (rid, n))

print("pre-flight OK: 28 sources, 28 parts, 28 unique rIds\n")
print("%-6s %-7s %-22s %11s %11s  %-9s %-16s %-16s" %
      ("fig", "rId", "part", "src px", "embed px", "size", "extent old", "extent new"))
print("-" * 118)

total_old = total_new = 0
for fig, rid, part, src in JOBS:
    spath = os.path.join(SRC, src)
    opath = os.path.join(UP, part)
    ext = os.path.splitext(part)[1].lower()
    old_sz = os.path.getsize(opath)

    im = Image.open(spath)
    w0, h0 = im.size
    aspect = w0 / h0

    if ext == ".gif":
        # animated -- copy the bytes so every frame survives
        shutil.copyfile(spath, opath)
        nw, nh = w0, h0
    else:
        scale = min(1.0, LONG_SIDE / max(w0, h0))
        nw, nh = max(1, round(w0 * scale)), max(1, round(h0 * scale))
        im2 = im.convert("RGB") if ext in (".jpg", ".jpeg") else im
        if (nw, nh) != (w0, h0):
            im2 = im2.resize((nw, nh), Image.LANCZOS)
        if ext in (".jpg", ".jpeg"):
            im2.save(opath, "JPEG", quality=90, optimize=True)
        else:
            im2.save(opath, "PNG", optimize=True)
    new_sz = os.path.getsize(opath)
    total_old += old_sz; total_new += new_sz

    # ---- refit the displayed box ----
    i = xml.find('r:embed="%s"' % rid)
    s = xml.rfind("<w:drawing>", 0, i)
    e = xml.find("</w:drawing>", i) + len("</w:drawing>")
    d = xml[s:e]
    ocx, ocy = (int(v) for v in
                re.search(r'<wp:extent cx="(\d+)" cy="(\d+)"/>', d).groups())

    cx = TEXT_W
    cy = round(cx / aspect)
    if cy > TEXT_H:
        cy = TEXT_H
        cx = round(cy * aspect)

    d = re.sub(r'<wp:extent cx="\d+" cy="\d+"/>',
               '<wp:extent cx="%d" cy="%d"/>' % (cx, cy), d)
    d = re.sub(r'<a:ext cx="\d+" cy="\d+"/>',
               '<a:ext cx="%d" cy="%d"/>' % (cx, cy), d)
    xml = xml[:s] + d + xml[e:]

    print("%-6s %-7s %-22s %5dx%-5d %5dx%-5d  %4dK>%4dK %6.2fx%-5.2f in %6.2fx%-5.2f in" %
          (fig, rid, part.replace("word/media/", ""), w0, h0, nw, nh,
           old_sz // 1024, new_sz // 1024,
           ocx / EMU, ocy / EMU, cx / EMU, cy / EMU))

open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("-" * 118)
print("media total %.1f MB -> %.1f MB" % (total_old / 1e6, total_new / 1e6))
print("document.xml rewritten (%d bytes)" % len(xml.encode()))
