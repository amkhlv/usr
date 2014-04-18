#!/usr/bin/python

import markdown
import sys

with open(sys.argv[1],'r') as fh:
    md = markdown.Markdown(extensions=['toc'])
    orig = fh.read()
    html = md.convert(orig)
    toc  = md.toc
    print("""
<!DOCTYPE html>
<html>
<body>
<meta charset=\"utf-8\">'<meta charset=\"utf-8\">
""")
    print(toc)
    print(html)
    print("""
</html>
</body>
""")
    
