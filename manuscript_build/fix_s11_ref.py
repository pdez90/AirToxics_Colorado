"""The main text points readers to 'Figure S1.1' for the per-day route maps.

No Figure S1.1 exists: section S1 is 'Mobile Instruments' and contains only
Tables S1.1/S1.2. The routes figure is Figure S3.1. The stale label traces to
the generating script's own name (37_creating_gif_figure_s1_1.R) -- the
figure was S1.1 before the SI was renumbered, and this cross-reference was never
updated. Corrected in red.
"""
import re
UP = "/tmp/msfix/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

n = len(re.findall(r'Figure\s*S1\.1\b', xml))
print("occurrences of 'Figure S1.1':", n)
if n != 1:
    raise SystemExit("expected exactly 1")

m = re.search(r'<w:r(?: [^>]*)?>(?:(?!</w:r>).)*?Figure\s*S1\.1(?:(?!</w:r>).)*?</w:r>',
              xml, re.S)
if not m:
    raise SystemExit("label spans multiple runs -- rerun merge_runs.py")
r = m.group(0)
rprm = re.search(r'<w:rPr>.*?</w:rPr>', r, re.S)
rpr = rprm.group(0) if rprm else "<w:rPr/>"
red = (rpr.replace("</w:rPr>", '<w:color w:val="FF0000"/></w:rPr>')
       if "<w:color" not in rpr else rpr)
t = re.search(r'<w:t(?: [^>]*)?>(.*?)</w:t>', r, re.S).group(1)
mm = re.search(r'Figure\s*S1\.1', t)
before, after = t[:mm.start()], t[mm.end():]

mk = lambda rp, s: '<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>' % (rp, s)
repl = (mk(rpr, before) if before else "") + mk(red, "Figure S3.1") + \
       (mk(rpr, after) if after else "")
xml = xml[:m.start()] + repl + xml[m.end():]
open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("Figure S1.1 -> Figure S3.1 (red)")
print("context:", re.sub(r'\s+', ' ', (before[-70:] + "[Figure S3.1]" + after[:70])))
