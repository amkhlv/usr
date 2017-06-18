#!/usr/bin/env python3

import markdown
import sys
import linkmd

with open(sys.argv[1],'r') as fh:
    md = markdown.Markdown(extensions=[linkmd.MdLinkExtension(), 'toc'])
#    orig = fh.read().decode('utf-8') 
#    html = md.convert(orig)
    html = md.convert(fh.read())
    toc  = md.toc
    print("""
<!DOCTYPE html>
<html>
<body>
<meta charset=\"utf-8\">
""")
    print(toc)
    print(html)
    print("""
</html>
</body>
""")
    
