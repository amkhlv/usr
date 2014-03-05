#!/usr/bin/python
# coding: utf-8

__author__    = "Andrei Mikhailov"
__copyright__ = "Copyright 2013, Andrei Mikhailov"
__license__   = "GPL"

"""My glorious email indexer"""

help_header = " Mail indexer; examples: \n\
 %prog -t mymail-from-somebody -m 21341231234@gmail.com 43223523@gmail.com 423131241234@yahoo\n\
 --- this opens three messages with these msgIDs in mutt\n\
 %prog -l 21341231234@gmail.com\n\
 --- this prints the filename of the file containing this messageid\n\
     (permanent part of the filename only!)" + "\n"

import sqlite3
import sys
import os
import re
from email.parser import Parser
import email.Header
import email.Message
from optparse import OptionParser
from glob import glob
import subprocess
import datetime
import dateutil.parser
import fileinput


dbfile = "/home/andrei/a/maildirs/mymail.db"
mailpath = "/home/andrei/a/maildirs/"
myroot = os.readlink("/home/andrei/a")

rex_from = re.compile(r"^From: (.*)")
rex_messageid = re.compile(r"^Message-ID: <(.*)>", flags=re.IGNORECASE)
rex_subject = re.compile(r"^Subject: (.*)")
rex_tstamp = re.compile(r"^(\d+)\..+")
rex_fname = re.compile(r"(.+\:)[^\:]*")

parser = OptionParser(help_header)


def command_line_arguments(parser):
    """Invocation"""
    parser.add_option("-r", "--rebuild", dest="rebuild", default=False, action="store_true",
                      help="""delete all entries in the database; rebuild the database from start""")
    parser.add_option("-f", "--forced", dest="forced", default=False, action="store_true",
                      help="""proceed even if there are new mails""")
    parser.add_option("-m", "--mutt", dest="msgid", default=False, action="store_true",
                      help="open in mutt the emails with Message IDs remaining arguments\n see also option --tmp",
                      metavar="MSGID")
    parser.add_option("--tombox", dest="convert_to_this_mbox", 
                      help="given a list of MessageID on stding, add them all to the mbox MBOXNAME", metavar="MBOXNAME")
    parser.add_option("--just-prep-tmp", dest="just_prep_maildir", action="store_true",
                      help="prepare the maildir with symlinks to Message IDs remaining arguments \n" +
                           "and print its path to stdout; see also the option --tmp")
    parser.add_option("-t", "--tmp", dest="tmpdirname",
                      help="name of the folder /tmp/mymail/TMPDIRNAME for the --mutt action\n" +
                           "otherwize /tmp/mymail/default will be used")
    parser.add_option("-l", "--locate", dest="msgid2locate",
                      help="print out the permanent part of the filename for Message ID MSGID",
                      metavar="MSGID")
    parser.add_option("-c", "--clean", dest="clean", default=False, action="store_true",
                      help="""delete /tmp/mymail""")
    parser.add_option("-i", "--info", dest="info4file", default=False, action="store_true",
                      help="""print from and subject of filenames which are remaining arguments""")
    parser.add_option("--decorate", dest="decorate", default=False, action="store_true",
                      help="""print decorated when showing info""")
    parser.add_option("-s", "--sqlite", dest="sqlite", help="execute sqlite statement, for example: " + "\n" +
                                                      "select * from sqlite_master  --- this is to see all tables")
    parser.add_option("-w", "--where", dest="criterium",
                      help="""print the msgids for those satisfying the where criterium""" +
                      """ e.g. --where "to_whom like '%somename%' and mime like '%pdf%'""" +
                      """ e.g. --where "from_whom like '%foo.bar%' and subject like '%something%'" """ + "\n" +
                      """ e.g. --where "betw('2012-09-21','2012-09-25',timestamp)" """ + "\n" +
                      """ or equiv: --where "timestamp between ts('2012-09-21') and ts('2012-09-25')" """ + "\n" +
                      """ (here "betw" and "ts" are my custom functions which I defined) """ + "\n" +
                      """ Or simply "timestamp > ts(date('now', '-5 days'))" """ + "\n" +
                      """ More examples: http://sqlite.org/lang_datefunc.html """ + "\n" +
                      """ Column names are: """ + "\n" +
                      """  (file, timestamp, messageid, from_whom, to_whom, cc, bcc, subject, header_date, xlabel, mime, tags) """)


class EmailMeta(object):
    def __init__(self, messageid, from_whom, to_whom, cc, bcc, timestamp, subject, header_date, xlabel, content_types):
        self.messageid = messageid
        self.from_whom = from_whom
        self.to_whom   = to_whom
        self.cc = cc
        self.bcc = bcc
        self.timestamp = timestamp
        self.subject = subject
        self.header_date = header_date
        self.xlabel = xlabel
        self.content_types = content_types


def prep_message(filehandle):
    return Parser().parse(filehandle, headersonly=False)

def get_content_types(message):
    if message.is_multipart():
        payload = message.get_payload()
        return [pt.get_content_type()  for pt in payload]
    else: return []

def is_maildir(dpath):
    return ( os.path.isdir(os.path.join(dpath, 'cur')) and
             os.path.isdir(os.path.join(dpath, 'new')) and
             os.path.isdir(os.path.join(dpath, 'tmp')) )


def list_files():
    main_maildirs = [os.path.join(mailpath, d) for d in os.listdir(mailpath)
                     if is_maildir(os.path.join(mailpath, d))]
    other_places = [m for m, _, _ in os.walk(myroot)
                    if os.path.basename(m) == 'maildir' and
                       is_maildir(m)]
    maildirs = main_maildirs + other_places
    print(maildirs)
    curmails = [os.path.join(md, "cur", f) for md in maildirs for f in os.listdir(os.path.join(md, "cur"))
                if os.path.isfile(os.path.join(md, "cur", f))]
    newmails = [os.path.join(md, "new", f) for md in maildirs for f in os.listdir(os.path.join(md, "new"))
                if os.path.isfile(os.path.join(md, "new", f))]
    tmpmails = [os.path.join(md, "tmp", f) for md in maildirs for f in os.listdir(os.path.join(md, "tmp"))
                if os.path.isfile(os.path.join(md, "tmp", f))]
    places = {'maildirs': maildirs, 'curmails': curmails, 'newmails': newmails, 'tmpmails': tmpmails}
    return places


def between(d1, d2, ts):
    n1 = datetime.date(*map(int, d1.split("-"))).strftime('%s')
    n2 = datetime.date(*map(int, d2.split("-"))).strftime('%s')
    if ( int(n1) < int(ts) ) and (int(n2) > int(ts)):
        return 1
    else:
        return 0

def sqlite_timestamp(d):
    try:
        x = (dateutil.parser.parse(d)).strftime('%s')
        return x
    except Exception as err:
        try:
            d1 = " ".join((d.split())[:5])
            return (dateutil.parser.parse(d1)).strftime('%s')
        except Exception as err1:
            print("*** ERROR parsing datetime :  " + str(err) + " >>> " + d)
            return datetime.datetime.now().strftime('%s')

def safety_check(places, forced):
    if (places['newmails'] or places['tmpmails']) and (not forced):
        print(places['newmails'])
        print(places['tmpmails'])
        sys.exit("""*** REFUSE: there are some either new or tmp emails, look at them first ***
  (or use option -f or --forced)""")


def timestamp(mailfile):
    bname = os.path.basename(mailfile)
    rm = re.match(rex_tstamp, bname)
    if rm:
        return rm.group(1)


def permname(mailfile):
    rm = re.match(rex_fname, mailfile)
    return rm.group(1)

def messageid(prsr):
    msgid = prsr['message-id']
    if msgid:
        return msgid
    else:
        print("------------------------------------MESSAGE ID NOT FOUND-------------------------------------------")

def decode_part_of_header(x,enc):
    if enc:
        try:
            answer = x.decode(enc).encode('utf-8')
            #answer = x.decode(enc)
        except Exception:
            answer = u"ERROR_DECODING-ENCODING"
            print(answer)
    else:
        try:
            answer = x.encode('utf-8')
            #answer = x
        except Exception:
            answer = u"ERROR_ENCODING"
            print(answer)
    return answer

def get_header_utf(msg, name):
    if msg[name]:
        parts = email.Header.decode_header(msg[name])
        if parts:
            return ",".join([decode_part_of_header(x[0],x[1]) for x in parts])
        else:
            print("---" + name +"---  FIELD NOT FOUND")
            return u""
    else: return u""


def emailfiles(msgid):
    """This looks up mailfile from msgid"""
    link = sqlite3.connect(dbfile)
    curs = link.cursor()
    curs.execute("""select file from mail where messageid like ?""", ("""%""" + msgid + """%""",))
    results = curs.fetchone()
    curs.close()
    link.close()
    return results


def datestr_to_int(ts):
    tss = ts.split()
    months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    m = months.index(tss[2]) if ( tss[2] in months ) else 12
    return 100 * m + int(tss[1])

def build_link():
    link = sqlite3.connect(dbfile)
    link.text_factory = str
    link.create_function("betw", 3, between)
    link.create_function("ts",1,sqlite_timestamp)
    return link

def exec_and_print_results(q):
    link = build_link()
    curs = link.cursor()
    curs.execute(q)
    for a in curs:
        print(a)
    curs.close()
    link.close()

def exec_query(q):
    link = build_link()
    curs = link.cursor()
    com = """select messageid from mail where """ + q
    print(com)
    curs.execute(com)
    a_msgids = [a[0].lstrip('<').rstrip('>') for a in curs]
    for a in a_msgids:
        print(a)
    curs.close()
    link.close()
    return a_msgids


def smart_truncate(cont, length=60, suffix='...'):
    content = cont.replace("\n", "<>")
    if len(content) <= length:
        return content
    else:
        return content[:length].rsplit(' ', 1)[0] + suffix


def meta_from_parser_and_timestamp(msg, mid, tstamp):
    return EmailMeta(
        messageid=mid,
        from_whom=get_header_utf(msg, 'from'),
        to_whom = get_header_utf(msg, 'to'),
        cc = get_header_utf(msg,'cc'),
        bcc = get_header_utf(msg, 'bcc'),
        timestamp=tstamp,
        subject=get_header_utf(msg, 'subject'),
        header_date=get_header_utf(msg, 'date'),
        xlabel= get_header_utf(msg,'x-label'),
        content_types=get_content_types(msg)
    )


def rebuild(places):
    """This deletes everything!"""
    link = build_link()
    curs = link.cursor()
    curs.execute("delete from mail")
    link.commit()
    curs.execute("vacuum")
    link.commit()
    for xi in places['curmails']:
        print(xi)
        tstamp = timestamp(xi)
        if not tstamp:
            print(" *** TIMESTAMP NOT FOUND *** ")
            break
        fh = open(xi, 'r')
        msg = prep_message(fh)
        mid = messageid(msg)
        eml = meta_from_parser_and_timestamp(msg, mid, tstamp)
        if mid:
            try:
                curs.execute(
                    """insert into mail """ +
                    """  (file, timestamp, messageid, from_whom, to_whom, cc, bcc, subject, header_date, xlabel, mime, tags) """ +
                    """  values (?,?,?,?,?,?,?,?,?,?,?,?) """,
                    (
                        permname(xi),
                        eml.timestamp,
                        eml.messageid,
                        eml.from_whom,
                        eml.to_whom,
                        eml.cc,
                        eml.bcc,
                        eml.subject,
                        eml.header_date,
                        eml.xlabel,
                        ",".join(eml.content_types),
                        ""
                    )
                )
            except Exception as err:
                sys.stderr.write('* ERROR: %s : %s %s\n' % (xi, eml.from_whom + eml.subject + mid, str(err)))
        fh.close()
    link.commit()
    curs.close()
    link.close()


def index(places):
    link = build_link()
    curs = link.cursor()
    curs.execute("select max(timestamp) from mail")
    tmax = int(curs.fetchone()[0])
    for xi in places['curmails']:
        if int(timestamp(xi)) < ( tmax + 1 ):
            continue
        print(xi)
        tstamp = timestamp(xi)
        if not tstamp:
            print(" *** ERROR: TIMESTAMP NOT FOUND *** ")
            break
        fh = open(xi, 'r')
        msg = prep_message(fh)
        mid = messageid(msg)
        eml = meta_from_parser_and_timestamp(msg, mid, tstamp)
        if mid:
            # print("---".join(eml.content_types))
            curs.execute("select file from mail where messageid like ?", (mid,))
            existing = curs.fetchone()
            if existing:
                print("-- already exists such Message-ID: " + mid)
                print("=> UPDATING ")
                try:
                    curs.execute(
                        "update mail set " +
                        " file = ?, timestamp = ?, from_whom = ?, to_whom = ?, cc = ?, bcc=?, subject = ?, tags = ?, header_date = ?, xlabel = ? , mime = ? " +
                        " where messageid like ? ",
                        (
                            permname(xi),
                            tstamp,
                            eml.from_whom,
                            eml.to_whom,
                            eml.cc,
                            eml.bcc,
                            eml.subject, "",
                            eml.header_date,
                            eml.xlabel,
                            ",".join(eml.content_types),
                            mid
                        )
                    )
                except Exception as err:
                    sys.stderr.write('* ERROR: %s : %s %s\n' % (xi, eml.from_whom + eml.subject + mid, str(err)))
            else:
                try:
                    curs.execute(
                        """insert into mail """ +
                        """  (file, timestamp, messageid, from_whom, to_whom, cc, bcc, subject, header_date, xlabel, mime, tags) """ +
                        """  values (?,?,?,?,?,?,?,?,?,?,?,?) """,
                        (
                            permname(xi),
                            tstamp,
                            mid,
                            eml.from_whom,
                            eml.to_whom,
                            eml.cc,
                            eml.bcc,
                            eml.subject,
                            eml.header_date,
                            eml.xlabel,
                            ",".join(eml.content_types),
                            ""
                        )
                    )
                except Exception as err:
                    sys.stderr.write('* ERROR: %s : %s %s\n' % (xi, eml.from_whom + eml.subject + mid, str(err)))
        fh.close()
    link.commit()
    curs.close()
    link.close()


def prepare_maildir(tmpdirname, args):
    tmpdir = "mymail/default"
    if tmpdirname:
        tmpdir = "mymail/" + tmpdirname
    else:
        tmpdir = "mymail/PID-" + str(os.getpid())
    os.system("""[ -d "/tmp/""" + tmpdir + """" ] && find "/tmp/""" + tmpdir + """" -delete""")
    os.system("""[ -d "/tmp/""" + tmpdir + """" ] || { mkdir -p """ +
              """ "/tmp/""" + tmpdir + "\""
                                       """ "/tmp/""" + tmpdir + """/new" """ +
              """ "/tmp/""" + tmpdir + """/cur" """ +
              """ "/tmp/""" + tmpdir + """/tmp" ; }""")
    for msgid1 in args:
        msgid = msgid1.rstrip()
        print(" >>>" + msgid + "<<<")
        fls = emailfiles(msgid.rstrip())
        if not fls: continue
        for fl in sum([glob(fl1 + "*") for fl1 in fls],[]):
            try:
                os.symlink(fl, """/tmp/""" + tmpdir + """/cur/""" + os.path.basename(fl))
            except OSError as err:
                print(str(err))
                print("Something wrong, maybe duplicate email?  msgid=" + msgid + " , emailfile=" + fl)
    return "/tmp/" + tmpdir


def main():
    command_line_arguments(parser)
    (options, args) = parser.parse_args()
    if options.rebuild:
        places = list_files()
        safety_check(places, options.forced)
        rebuild(places)
    elif options.sqlite:
        exec_and_print_results(options.sqlite)
    elif options.msgid:
        if options.criterium:
            subprocess.call(["mutt", "-f", prepare_maildir(options.tmpdirname, exec_query(options.criterium))])
        else:
            subprocess.call(["mutt", "-f", prepare_maildir(options.tmpdirname, args)])
    elif options.just_prep_maildir:
        print(prepare_maildir(options.tmpdirname, args))
    elif options.msgid2locate:
        for ef in emailfiles(options.msgid2locate):
            print(ef)
    elif options.clean:
        os.system("""find /tmp/mymail -delete""")
    elif options.info4file:
        sprtr = u"." if options.decorate else u"." # for now options.decorate does nothing
        sprtr1 = u"  "
        lines_to_print = []
        for x in args:
            fh = open(x, 'r')
            msg = prep_message(fh)
            try:
                d_str = get_header_utf(msg,'date')[:11]
                dt_int = datestr_to_int(d_str)
            except Exception as err:
                d_str = "-----------"
                dt_int = 10000
            f_str = smart_truncate(get_header_utf(msg,'from') or "---", 30)
            s_str = smart_truncate(get_header_utf(msg,'subject') or "---")
            lines_to_print.append([dt_int,
                                   d_str + sprtr1 + f_str + "." * (25 - len(f_str)) + sprtr + s_str])
            fh.close()
        for u in sorted(lines_to_print, key=lambda v: v[0]):
            print(u[1])
    elif options.criterium:
        exec_query(options.criterium)
    elif options.convert_to_this_mbox:
        idlist = list(sys.stdin)
        for message_id in idlist:
            print "\n Processing " + message_id.rstrip()
            efs = emailfiles(message_id.rstrip())
            if not(efs): efs = []
            for ef in efs:
                print("adding to MBOX " + options.convert_to_this_mbox + " the email file: " + ef)
                os.system("formail < " + glob(ef + "*")[0] + " >> " + options.convert_to_this_mbox)
    else:
        places = list_files()
        safety_check(places, options.forced)
        index(places)


if __name__ == "__main__":
    main()
