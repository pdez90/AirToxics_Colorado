"""Replace a phrase with red-coloured text inside a Word document part.

Splits the containing <w:r> into (before | new-in-red | after) so only the
changed words turn red. CT_RPr is a sequence, so <w:color/> goes before
<w:vertAlign>/<w:rtl>/<w:cs>/<w:lang> rather than being appended.
"""
import re

LATER = ("spacing","w","kern","position","sz","szCs","highlight","u","effect",
         "bdr","shd","fitText","vertAlign","rtl","cs","em","lang",
         "eastAsianLayout","specVanish","oMath")


def _anchor(body):
    best = -1
    for t in LATER:
        for pat in ('<w:%s ' % t, '<w:%s/>' % t):
            j = body.find(pat)
            if j >= 0 and (best < 0 or j < best):
                best = j
    return best


def make_red(rpr):
    if not rpr or rpr == "<w:rPr/>":
        return '<w:rPr><w:color w:val="FF0000"/></w:rPr>'
    body = rpr[len("<w:rPr>"):-len("</w:rPr>")]
    body = re.sub(r'<w:color [^/>]*/>', '', body)
    a = _anchor(body)
    col = '<w:color w:val="FF0000"/>'
    return "<w:rPr>" + (body + col if a < 0 else body[:a] + col + body[a:]) + "</w:rPr>"


def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def red_replace(xml, old, new, label="", start=0):
    """Replace the first occurrence of plain-text `old` at/after char `start`."""
    o = esc(old)
    m = re.search(r'<w:r(?: [^>]*)?>(?:(?!</w:r>).)*?' + re.escape(o) +
                  r'(?:(?!</w:r>).)*?</w:r>', xml[start:], re.S)
    if not m:
        raise SystemExit("[%s] not found in a single run: %r" % (label, old[:80]))
    s0, e0 = start + m.start(), start + m.end()
    r = xml[s0:e0]
    rm = re.search(r'<w:rPr>.*?</w:rPr>', r, re.S)
    rpr = rm.group(0) if rm else "<w:rPr/>"
    red = make_red(rpr)
    t = re.search(r'<w:t(?: [^>]*)?>(.*?)</w:t>', r, re.S).group(1)
    before, after = t.split(o, 1)
    mk = lambda rp, s: '<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>' % (rp, s)
    repl = ((mk(rpr, before) if before else "") + mk(red, esc(new)) +
            (mk(rpr, after) if after else ""))
    print("  [%s]\n      - %s\n      + %s" % (label, old[:150], new[:150]))
    return xml[:s0] + repl + xml[e0:]


def red_replace_after(xml, cue, old, new, label=""):
    """Replace the first `old` that appears after the literal `cue`."""
    i = xml.index(esc(cue))
    return red_replace(xml, old, new, label=label, start=i)


def count(xml, s):
    return len(re.findall(re.escape(esc(s)), xml))
