#!/usr/bin/env python3
"""
75_extract_airtoxscreen.py  (2026-09-22)

Pull the five columns the analysis needs out of EPA's 2020 AirToxScreen
block-level ambient-concentration workbook for EPA Region 8:

    State | Block | Population | BENZENE | TOLUENE | XYLENES (MIXED ISOMERS)

The workbook is 183 columns wide and ~1.9 GB when unzipped, so it is streamed
straight out of the .xlsx rather than loaded (readxl would need many GB of RAM
to hold it as text). Colorado rows only.

    python3 75_extract_airtoxscreen.py <Region8...xlsx> <out.csv>

Output: a CSV with Block (15-digit, zero-padded), Population and the three
pollutants in ug/m3. R_scripts/75_airtoxscreen_from_epa.R turns that into
airtoxscreen.xlsx / airtoxscreen.csv and checks it against the manuscript run.
"""
import csv, re, sys, zipfile

WANT = {"BLOCK", "POPULATION", "BENZENE", "TOLUENE", "XYLENES (MIXED ISOMERS)", "STATE"}
CELL = re.compile(rb'<c r="([A-Z]+)\d+"[^>]*?>(?:<is><t[^>]*>(.*?)</t></is>|<v>(.*?)</v>)?', re.S)
ROW = re.compile(rb"<row[^>]*?>.*?</row>", re.S)


def cells(row_xml):
    out = {}
    for col, inline, v in CELL.findall(row_xml):
        out[col.decode()] = (inline or v or b"").decode("utf8", "replace")
    return out


def main(src, dst):
    z = zipfile.ZipFile(src)
    sheet = [n for n in z.namelist() if n.startswith("xl/worksheets/sheet")][0]
    colmap, n_in, n_out, after_co = None, 0, 0, 0
    with z.open(sheet) as f, open(dst, "w", newline="") as fo:
        w = csv.writer(fo)
        w.writerow(["Block", "Population", "BENZENE", "TOLUENE", "XYLENES (MIXED ISOMERS)"])
        buf = b""
        while True:
            chunk = f.read(1 << 22)
            if not chunk:
                break
            buf += chunk
            last = 0
            for m in ROW.finditer(buf):
                last = m.end()
                c = cells(m.group(0))
                if colmap is None:                      # header row
                    colmap = {v.upper(): k for k, v in c.items() if v.upper() in WANT}
                    missing = WANT - set(colmap)
                    if missing:
                        sys.exit("header is missing: " + ", ".join(sorted(missing)))
                    print("[extract] columns:", colmap, flush=True)
                    continue
                n_in += 1
                if c.get(colmap["STATE"], "").strip().upper() != "CO":
                    if n_out:
                        after_co += 1
                        if after_co > 5000:             # Colorado block is over; stop streaming
                            print("[extract] past the Colorado rows - stopping early", flush=True)
                            buf = b""
                            break
                    continue
                after_co = 0
                blk = re.sub(r"\D", "", c.get(colmap["BLOCK"], "")).rjust(15, "0")
                w.writerow([blk, c.get(colmap["POPULATION"], ""), c.get(colmap["BENZENE"], ""),
                            c.get(colmap["TOLUENE"], ""), c.get(colmap["XYLENES (MIXED ISOMERS)"], "")])
                n_out += 1
                if n_out % 25000 == 0:
                    print(f"[extract] {n_in:,} rows read, {n_out:,} Colorado blocks written", flush=True)
            if after_co > 5000:
                break
            buf = buf[last:]
    print(f"[extract] DONE: {n_in:,} Region 8 blocks read, {n_out:,} Colorado blocks -> {dst}", flush=True)
    if n_out < 50000:
        sys.exit(f"only {n_out} Colorado blocks - expected ~100,000 populated Colorado blocks; check the workbook")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
