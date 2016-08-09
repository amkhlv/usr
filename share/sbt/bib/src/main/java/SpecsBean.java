


import java.util.ArrayList;
import java.util.List;

/**
 * Created by andrei on 18/07/14.
 */
public class SpecsBean {
    private ArrayList<String> regexTitle;
    private ArrayList<String> regexTitleExclude;
    private ArrayList<String> regexAuthors;
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

    public ArrayList<String> getRegexTitle() {
        return regexTitle;
    }

    public void setRegexTitle(ArrayList<String> as) {
        this.regexTitle = as;
    }

    public ArrayList<String> getRegexTitleExclude() {
        return regexTitleExclude;
    }

    public void setRegexTitleExclude(ArrayList<String> as) {
        this.regexTitleExclude = as;
    }

    public ArrayList<String> getRegexAuthors() { return regexAuthors; }

    public void setRegexAuthors(ArrayList<String> as) { this.regexAuthors = as; }

    public String getBibstyle() { return bibstyle; }

    public void setBibstyle(String a) { this.bibstyle = a; }


}
