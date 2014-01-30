.. amkhlv documentation master file, created by
   sphinx-quickstart on Mon Feb 18 16:24:02 2013.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

clndr
=====

**To copy from buffer:**  middle mouse button **with Shift**

Recurrent events::

  recur      = "FREQ"=freq *(

             ; either UNTIL or COUNT may appear in a 'recur',
             ; but UNTIL and COUNT MUST NOT occur in the same 'recur'

             ( ";" "UNTIL" "=" enddate ) /
             ( ";" "COUNT" "=" 1*DIGIT ) /

             ; the rest of these keywords are optional,
             ; but MUST NOT occur more than once

             ( ";" "INTERVAL" "=" 1*DIGIT )          /
             ( ";" "BYSECOND" "=" byseclist )        /
             ( ";" "BYMINUTE" "=" byminlist )        /
             ( ";" "BYHOUR" "=" byhrlist )           /
             ( ";" "BYDAY" "=" bywdaylist )          /
             ( ";" "BYMONTHDAY" "=" bymodaylist )    /
             ( ";" "BYYEARDAY" "=" byyrdaylist )     /
             ( ";" "BYWEEKNO" "=" bywknolist )       /
             ( ";" "BYMONTH" "=" bymolist )          /
             ( ";" "BYSETPOS" "=" bysplist )         /
             ( ";" "WKST" "=" weekday )              /
             ( ";" x-name "=" text )
             )

  freq       = "SECONDLY" / "MINUTELY" / "HOURLY" / "DAILY"
             / "WEEKLY" / "MONTHLY" / "YEARLY"

  enddate    = date
  enddate    =/ date-time            ;An UTC value

  byseclist  = seconds / ( seconds *("," seconds) )

  seconds    = 1DIGIT / 2DIGIT       ;0 to 59

  byminlist  = minutes / ( minutes *("," minutes) )

  minutes    = 1DIGIT / 2DIGIT       ;0 to 59

  byhrlist   = hour / ( hour *("," hour) )

  hour       = 1DIGIT / 2DIGIT       ;0 to 23

  bywdaylist = weekdaynum / ( weekdaynum *("," weekdaynum) )

  weekdaynum = [([plus] ordwk / minus ordwk)] weekday

  plus       = "+"

  minus      = "-"

  ordwk      = 1DIGIT / 2DIGIT       ;1 to 53

  weekday    = "SU" / "MO" / "TU" / "WE" / "TH" / "FR" / "SA"
  ;Corresponding to SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY,
  ;FRIDAY, SATURDAY and SUNDAY days of the week.

  bymodaylist = monthdaynum / ( monthdaynum *("," monthdaynum) )

  monthdaynum = ([plus] ordmoday) / (minus ordmoday)

  ordmoday   = 1DIGIT / 2DIGIT       ;1 to 31

  byyrdaylist = yeardaynum / ( yeardaynum *("," yeardaynum) )

  yeardaynum = ([plus] ordyrday) / (minus ordyrday)

  ordyrday   = 1DIGIT / 2DIGIT / 3DIGIT      ;1 to 366

  bywknolist = weeknum / ( weeknum *("," weeknum) )
  weeknum    = ([plus] ordwk) / (minus ordwk)

  bymolist   = monthnum / ( monthnum *("," monthnum) )

  monthnum   = 1DIGIT / 2DIGIT       ;1 to 12

  bysplist   = setposday / ( setposday *("," setposday) )

  setposday  = yeardaynum


.. automodule:: clndr
   :members: command_line_arguments


* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

