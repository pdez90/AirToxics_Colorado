"""CT_RPr is a sequence, not a bag: <w:color> must precede <w:vertAlign>,
<w:rtl>, <w:cs>, <w:lang>. Two runs written today inserted the colour after
<w:rtl>; Word rejects that ordering. Move it into place.
"""
import re, sys

LATER = ("spacing","w","kern","position","sz","szCs","highlight","u","effect",
         "bdr","shd","fitText","vertAlign","rtl","cs","em","lang",
         "eastAsianLayout","specVanish","oMath")


def first_later(body):
    best = -1
    for t in LATER:
        for pat in ('<w:%s ' % t, '<w:%s/>' % t):
            j = body.find(pat)
            if j >= 0 and (best < 0 or j < best):
                best = j
    return best


def fix(path):
    x = open(path, encoding="utf-8").read()
    out, last, n = [], 0, 0
    for m in re.finditer(r'<w:rPr>(.*?)</w:rPr>', x, re.S):
        body = m.group(1)
        cm = re.search(r'<w:color [^/>]*/>', body)
        if not cm:
            continue
        anchor = first_later(body)
        if anchor < 0 or anchor > cm.start():
            continue
        stripped = body[:cm.start()] + body[cm.end():]
        anchor = first_later(stripped)
        newbody = stripped[:anchor] + cm.group(0) + stripped[anchor:]
        out.append(x[last:m.start()] + "<w:rPr>" + newbody + "</w:rPr>")
        last = m.end()
        n += 1
        print("   fixed: %s\n      ->  %s" % (body[:110], newbody[:110]))
    out.append(x[last:])
    if n:
        open(path, "w", encoding="utf-8").write("".join(out))
    print("%s: %d rPr block(s) reordered" % (path, n))


for p in sys.argv[1:]:
    fix(p)
