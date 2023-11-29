#!/usr/bin/env python3
from time import sleep

from helium import start_firefox
from selenium.webdriver import FirefoxOptions
import argparse


parser = argparse.ArgumentParser(
                    prog='html2pdf',
                    description='prints html',
                    epilog='happy printing!')
parser.add_argument('-i','--input', help = 'URL')  
parser.add_argument('-o','--output', help='absolute (!) path to output filename')  
args = parser.parse_args()

options = FirefoxOptions()
options.add_argument("--start-maximized")
options.set_preference("print.always_print_silent", True)
#options.set_preference("print_printer", "Mozilla Save to PDF")
options.set_preference("print_printer", "Mozilla Save to PDF")
#options.set_preference("print.printer_Mozilla_Save_to_PDF.print_to_file", True)
options.set_preference("print.printer_Mozilla_Save_to_PDF.print_to_file", True)
options.set_preference("print.printer_Mozilla_Save_to_PDF.print_to_filename", args.output)
options.set_preference('print.save_as_pdf.links.enabled', True)

driver = start_firefox(args.input, options=options)

driver.execute_script("window.print();")
sleep(2)  # Found that a little wait is needed for the print to be rendered otherwise the file will be corrupted

driver.quit()
