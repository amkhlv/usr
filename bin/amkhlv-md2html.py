#!/usr/bin/python

import markdown
import sys

with open(sys.argv[1],'r') as fh:
    md = markdown.Markdown(extensions=['toc'])
    orig = fh.read()
    html = md.convert(orig)
    toc  = md.toc
    print("<HTML>")
    print(toc)
    print(html)
    print("</HTML>")
