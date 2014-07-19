

import scala.util.parsing.combinator.testing.Str;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by andrei on 18/07/14.
 */
public class SpecsBean {
    private ArrayList<String> regex;
    private String autodir;
    private String autofile;
    private String bibfile;
    private String bibstyle;

    public String getAutodir() {
        return autodir;
    }

    public void setAutodir(String a) {
        this.autodir = a;
    }

    public String getAutofile() {
        return autofile;
    }

    public void setAutofile(String a) {
        this.autofile = a;
    }

    public String getBibfile() {return bibfile; }

    public void setBibfile(String a) { this.bibfile  = a; }

    public ArrayList<String> getRegex() {
        return regex;
    }

    public void setRegex(ArrayList<String> as) {
        this.regex = as;
    }

    public String getBibstyle() { return bibstyle; }

    public void setBibstyle(String a) { this.bibstyle = a; }


}
