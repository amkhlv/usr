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

parser = argparse.ArgumentParser(
                    prog='html2pdf',
     formatter_class=RawTextHelpFormatter,
                    description=
    """
       prints html; the URLS should go line-by-line on stdin.
       The PDF output is determined by the --transformer function in hy lang. Example:
       -t "(fn [p] (+ p #[[.pdf]]))"
    """,
            epilog='happy printing!')
#parser.add_argument('-i','--input', help = 'URL')  
#parser.add_argument('-o','--output', help='absolute (!) path to output filename')  
parser.add_argument('-t', '--transformer', help = 'transformer function')  
parser.add_argument('-n', '--dry', action = "store_true", help='just show output paths')
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
options.set_preference('print.save_as_pdf.links.enabled', True)

driver = start_firefox('https://www.google.com', options=options)

for pin in sys.stdin:
    p = prep_path(pin)
    driver.get(pin.rstrip())
    driver.execute_script('window.print();')
    sleep(2)
    pout = hy.eval(hy.read_many(f'({args.transformer} p)'))
    shutil.move('/tmp/amkhlv-html2pdf.pdf',pout)

driver.quit()
