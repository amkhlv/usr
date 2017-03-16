from bs4 import BeautifulSoup, Tag, NavigableString
import os
import sys
from typing import List, Dict, Any
import xml.etree.ElementTree as ET
from selenium import webdriver

host = "http://inspirehep.net"
default_headers = {
    "Host": host,
    "User-Agent": "Mozilla/5.0 (X11; Linux x86_64; rv:45.0) Gecko/20100101 Firefox/45.0",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.5",
    "Accept-Encoding": "gzip, deflate"
}
driver = webdriver.PhantomJS()

navstr = NavigableString("hi")

def is_citation_stats(x: Tag):
    return x.has_attr("data-box-source") and x['data-box-source'] == "citations-summary"

if __name__ == "__main__":
    driver.implicitly_wait(30)
    driver.get(host + "/author/profile/A.Mikhailov.1")
    html = driver.page_source
    bs = BeautifulSoup(html, "html.parser")
    stats = bs.find(is_citation_stats)
    bodies = stats.select("table tbody")
    xroot = ET.Element("rows")
    for body in bodies:
        for tr in body.select("tr"):
            th = tr.find("th")
            tds = tr.find_all("td")
            if th:
                w = ET.SubElement(
                    xroot,
                    "row",
                    attrib={"Item": th.get_text(), "Citeable": tds[0].get_text(), "Published": tds[1].get_text()}
                )
            else:
                w = ET.SubElement(
                    xroot,
                    "row",
                    attrib={"Item": tds[0].get_text(), "Citeable": tds[1].get_text(), "Published": tds[2].get_text()}
                )
    sys.stdout.write(ET.tostring(xroot, encoding="unicode"))



