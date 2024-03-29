#!/usr/bin/env python3
from time import sleep

from helium import start_firefox
from selenium.webdriver import FirefoxOptions
import argparse
from argparse import RawTextHelpFormatter
import hy
import sys
import shutil
import re
import os

default_transformer="(fn [p] (+ p #[[.pdf]]))"
parser = argparse.ArgumentParser(
                    prog='html2pdf',
     formatter_class=RawTextHelpFormatter,
                    description=
    """
       prints html; the URLS should go line-by-line on stdin.
       The PDF output is determined by the --transformer function in hy lang. Example:
       -t """ + default_transformer + 
    """
       Transformer receives as argument the URI with stripped proto prefix (https:// or file:// is stripped)
       and, in case of https://, with / replaced with -- 
    """,
            epilog='happy printing!')
#parser.add_argument('-i','--input', help = 'URL')  
#parser.add_argument('-o','--output', help='absolute (!) path to output filename')  
parser.add_argument('-t', '--transformer', help = 'transformer function; default is ' + default_transformer, default=default_transformer)  
parser.add_argument('-n', '--dry', action = "store_true", help='just show output paths')
parser.add_argument('-d', '--delay', help = "delay in seconds before starting printint", default = 1)
args = parser.parse_args()

def prep_path(p):
    p = p.rstrip()
    filerx = re.compile('^file://')    
    if filerx.match(p):
        return filerx.sub('',p)
    else: 
        p = re.sub('^https?://','',p)
        p = p.replace('/','--')
        return p
    

if args.dry:
    for pin in sys.stdin:
        p = prep_path(pin)
        pout = hy.eval(hy.read_many(f'({args.transformer} p)'))
        print(pout)
    quit()
options = FirefoxOptions()
options.add_argument("--start-maximized")
options.set_preference("print.always_print_silent", True)
#options.set_preference("print_printer", "Mozilla Save to PDF")
options.set_preference("print_printer", "Mozilla Save to PDF")
#options.set_preference("print.printer_Mozilla_Save_to_PDF.print_to_file", True)
options.set_preference("print.printer_Mozilla_Save_to_PDF.print_to_file", True)
options.set_preference("print.printer_Mozilla_Save_to_PDF.print_to_filename", '/tmp/amkhlv-html2pdf.pdf')
options.set_preference("print.save_as_pdf.links.enabled", True)
options.set_preference("print.print_headercenter", "")
options.set_preference("print.print_headerleft","")
options.set_preference("print.print_headerright","")
options.set_preference("print.print_footercenter", "")
options.set_preference("print.print_footerleft","")
options.set_preference("print.print_footerright","")

driver = start_firefox('http://localhost/locals/', options=options)

for pin in sys.stdin:
    p = prep_path(pin)
    driver.get(pin.rstrip())
    sleep(int(args.delay))
    driver.execute_script('window.print();')
    sleep(1)
    while not os.path.exists('/tmp/amkhlv-html2pdf.pdf'): sleep(1)
    sleep(1)
    pout = hy.eval(hy.read_many(f'({args.transformer} p)'))
    shutil.move('/tmp/amkhlv-html2pdf.pdf',pout)

driver.quit()
