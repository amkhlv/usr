#!/usr/bin/python2

import vobject
import json
import sys

if __name__ == '__main__':
    jsn = json.load(sys.stdin)
    vc = vobject.vCard()
    a = vc.add('fn')
    a.value = (jsn['first'] + " " if 'first' in jsn.keys() else "") + jsn['last']
    a = vc.add('n')
    a.value = vobject.vcard.Name(family=jsn['last'], given=jsn['first'] if 'first' in jsn.keys() else "")
    if 'workphone' in jsn.keys() and jsn['workphone'] is not None:
        for x in jsn['workphone']:
            a = vc.add('tel')
            a.type_param = "work"
            a.value = x
    if 'homephone' in jsn.keys() and jsn['homephone'] is not None:
        for x in jsn['homephone']:
            a = vc.add('tel')
            a.type_param = "home"
            a.value = x
    if 'cellphone' in jsn.keys() and jsn['cellphone'] is not None:
        for x in jsn['cellphone']:
            a = vc.add('tel')
            a.type_param = "cell"
            a.value = x
    if 'birthday' in jsn.keys() and jsn['birthday'] is not None:
        a = vc.add('bday')
        a.value = jsn['birthday']
    if 'email' in jsn.keys() and jsn['email'] is not None:
        for x in jsn['email']:
            a = vc.add('email')
            a.value = x

    print(vc.serialize())

