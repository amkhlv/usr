import linii
import os

linii.my.dbfile= os.environ['HOME'] + '/a/tech/base/addr.db'

abk = {'tablename': 'abk',
'columns': [ ('last','Last Name',1, True ) ,
  ('first','First Name',1, True ) ,
  ('datecollected','Date When Collected',1, False ),
  ('email','Email',1, True ) ,
  ('workphone','Work phone',1 , True ) ,
  ('homephone','Home phone',1 , True ) ,
  ('fax','Fax',1 , True ) ,
  ('cellphone','Cell phone',1 , True ) ,
  ('homeaddress','Home address',5 , True ) ,
  ('workaddress','Work address',5 , True ) ,
  ('website','Website',1 , True ) ,
  ('country','Countries',1 , True) ,
  ('birthday','Birthday',1, True ) ,
  ('notes','Notes',2 , True ) ]
}

gtd = {'tablename': 'gtd',
'columns': [ ('task','Task',1, True ) ,
  ('a','State',1, True ) ,
  ('stage','Stage(012...)',1, True ) ,
  ('next','Next',3, True) ,
  ('time', 'Estimated time',1, True ) ,
  ('dependencies', 'Depends on',3, True ) ,
  ('notes','Notes',9, True ) ,
  ('deadline','Deadline',1, True ) ,
  ('postponed','Postponed until',1, True ) ,
  ('entered','Entered on',1, False ) ,
  ('tags','tags',1, True ) ]
}

monthly = {'tablename': 'monthly',
'columns': [ ('message','Message',1, True),
  ('months','Months',1,True),
  ('days','Days',1,True),
  ('advance','Warn in advance days',1,True),
  ('dismissed','Dismissed on',1, True),
  ('tmp_note','Temp note',9, True),
  ('note','Note',4, True) ]
}

