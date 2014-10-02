package controllers;

/**
 * Created by andrei on 30/09/14.
 */


import net.fortuna.ical4j.data.*;
import net.fortuna.ical4j.model.*;
import net.fortuna.ical4j.filter.*;
import net.fortuna.ical4j.model.property.Comment;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import play.api.Play;

public class ICalendar {
    public static Component[] getEventsForDay(java.util.Date d) {
        ArrayList<Component> result = new ArrayList<Component>();
        try {
            FileInputStream fin = new FileInputStream(Application.ics());
            CalendarBuilder builder = new CalendarBuilder();
            try {
                Calendar calendar = builder.build(fin);
                Period period = new Period(new DateTime(d), new Dur(1, 0, 0, 0));
                Rule[] rls = {new PeriodRule(period)};
                Filter filter = new Filter(rls, Filter.MATCH_ALL);
                Object[] a = filter.filter(calendar.getComponents(Component.VEVENT)).toArray();
                for (int i = 0; i < a.length; i++) result.add((Component) a[i]);
            } catch (ParserException pe) {

            } catch (IOException ioe) {

            }
        } catch (FileNotFoundException e) {

        }
        return result.toArray(new Component[result.size()]);
    }
}
