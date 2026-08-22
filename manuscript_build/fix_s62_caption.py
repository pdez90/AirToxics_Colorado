"""Figure S6.2's caption still described 'the last 7 plumes in time'.

The 2026-08-21 run retains 3 plumes (ids 6, 9, 28) and the regenerated figure
shows exactly three facets, so the caption is corrected -- in red -- to say all
three retained plumes are shown.
"""
import re
UP = "/tmp/sifig/unpacked"
xml = open(f"{UP}/word/document.xml", encoding="utf-8").read()

OLD = "selected as the last 7 plumes in time based on the timestamp of peak enhancement"
NEW = "all three retained plumes are shown, one per panel, labelled by plume identifier"

n = xml.count(OLD)
print("occurrences of stale phrase:", n)
if n != 1:
    raise SystemExit("expected exactly 1")

# locate the run holding the phrase, split it so only the new text is red
m = re.search(r'<w:r(?: [^>]*)?>(?:(?!</w:r>).)*?' + re.escape(OLD) +
              r'(?:(?!</w:r>).)*?</w:r>', xml, re.S)
if not m:
    raise SystemExit("phrase spans multiple runs -- rerun merge_runs.py")
r = m.group(0)
rpr = re.search(r'<w:rPr>.*?</w:rPr>', r, re.S)
rpr = rpr.group(0) if rpr else "<w:rPr/>"
red = rpr.replace("</w:rPr>", '<w:color w:val="FF0000"/></w:rPr>') \
      if "<w:color" not in rpr else rpr
t = re.search(r'<w:t(?: [^>]*)?>(.*?)</w:t>', r, re.S).group(1)
before, after = t.split(OLD)

def mk(rp, txt):
    return '<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>' % (rp, txt)

repl = ""
if before:
    repl += mk(rpr, before)
repl += mk(red, NEW)
if after:
    repl += mk(rpr, after)

xml = xml[:m.start()] + repl + xml[m.end():]
open(f"{UP}/word/document.xml", "w", encoding="utf-8").write(xml)
print("caption corrected; new text marked red")
print("  was:", OLD)
print("  now:", NEW)
