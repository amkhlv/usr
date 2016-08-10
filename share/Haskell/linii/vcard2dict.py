#!/usr/bin/python2

import vobject
import fileinput
import json


def format_address(avalue):
    return "\n".join([u for u in  [avalue.box, avalue.extended, avalue.street, avalue.city, avalue.region, avalue.code, avalue.country] if u != ''])

def getComponents(txt):
    """
    Reads the text  txt (a string), which can contain several concatenated vcards, ito the list of Components.
    Each vcard should start with BEGIN:VCARD and end with END:VCARD
    """
    return vobject.readComponents(txt)

def componentDict(v):
    """
    Converts component into dictionary
    """
    vadr = [] if not ('adr' in v.contents.keys()) else v.contents['adr']
    vtel = [] if not ('tel' in v.contents.keys()) else v.contents['tel']
    vemail = [] if not ('email' in v.contents.keys()) else v.contents['email']
    vorg = [] if not ('org' in v.contents.keys()) else v.contents['org']
    vtitle = [] if not ('title' in v.contents.keys()) else v.contents['title']
    workaddresses = [format_address(a.value) for a in vadr
                     if (('TYPE' in a.params.keys())
                         and (('WORK' in a.params['TYPE']) or ('work' in a.params['TYPE'])))]
    homeaddresses = [format_address(a.value) for a in vadr
                     if (not ('TYPE' in a.params.keys())
                         or ('HOME' in a.params['TYPE'])
                         or ('home' in a.params['TYPE']))]
    workphones = [u.value for u in vtel if (('TYPE' in u.params.keys())
                                            and (('WORK' in u.params['TYPE']) or ('work' in u.params['TYPE'])))]
    cellphones = [u.value for u in vtel if (('TYPE' in u.params.keys())
                                            and (('CELL' in u.params['TYPE']) or ('cell' in u.params['TYPE'])))]
    faxes = [u.value for u in vtel if (('TYPE' in u.params.keys())
                                            and (('FAX' in u.params['TYPE']) or ('fax' in u.params['TYPE'])))]
    homephones = [u.value for u in vtel if (not ('TYPE' in u.params.keys())
                                            or not (('WORK' in u.params['TYPE'])
                                                    or ('work' in u.params['TYPE'])
                                                    or ('CELL' in u.params['TYPE'])
                                                    or ('cell' in u.params['TYPE'])
                                                    or ('FAX' in u.params['TYPE'])
                                                    or ('fax' in u.params['TYPE']))
                                            or ('home' in u.params['TYPE']))]
    vd = {
    'last': ", ".join(u for u in [v.n.value.family, v.n.value.suffix] if u != ''),
    'first': " ".join(u for u in [v.n.value.prefix, v.n.value.given] if u != ''),
    }
    if workaddresses: vd['workaddresses'] = workaddresses
    if homeaddresses: vd['homeaddresses'] = homeaddresses
    if vemail: vd['emails'] = [u.value for u in vemail]
    if workphones: vd['workphones'] = workphones
    if cellphones: vd['cellphones'] = cellphones
    if faxes: vd['faxes'] = faxes
    if homephones: vd['homephones'] = homephones
    if vorg: vd['organizations'] = [u.value for u in vorg]
    if vtitle: vd['titles'] = [u.value for u in vtitle]
    if 'bday' in v.contents.keys(): vd['birthday'] = v.contents['bday'][0].value
    return vd

if __name__ == '__main__':
    x = "".join(fileinput.input())
    print json.dumps([componentDict(c) for c in getComponents(x)], indent=2)

