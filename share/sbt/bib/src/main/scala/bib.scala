

import java.io.{FileWriter, File, FileReader}
import java.util
import scala.collection.JavaConverters._
import org.jbibtex.{BibTeXEntry, Key, Value, BibTeXDatabase, BibTeXParser}
import scala.sys.process._
import scala.collection.mutable._


/**
 * This is a script to create the pdf file of the selected bibliography
 *
 * All the necessary parameters are fixed in `resources/specs.yaml` ; notice that there are several YAML files
 * in `resources/` , so one of them should be symlinked to `specs.yaml` ; the parameters are:
 *
 *  - `regex`    : this is a list of all regexes which should match
 *  - `autodir`  : this is the directory where all the LaTeX and PDF files will appear
 *  - `autofile` : this should be a reasonably exotic filename (no extension!)
 *  - `bibfile`  : path to the main bibliography file
 *  - `bibstyle` : e.g. `jhep` ; this means that the file `jhep.sty` '''should be present''' in the `autodir`
 *
 *  So, ''' be careful when cleaning the autodir '''
 *
 *  For some very strange reason, I have to modify the file `/usr/share/texlive/texmf-dist/web2c/texmf.cnf`
 *  replacing `openout_any = p` with `openout_any = a`
 */


object Bib {

  /**
   * This creates the pdf file of the selected bibliography
   */

  val yamlReader:  FileReader = new FileReader(new File("resources/specs.yaml"))

  val specs = Adaptor.getSB(yamlReader)

  val fr: FileReader = new FileReader(new File("/home/andrei/a/Work/andrei.bib"))
  val fNoExt: String = specs.getAutodir.concat("/").concat(specs.getAutofile)
  val fPreNoExt: String = fNoExt.concat("_pre")
  val fPre: String = fPreNoExt.concat(".tex")

  val preHeader: String =
    """
      |\documentclass[12pt]{article}
      |\begin{document}
      |
    """.stripMargin
  val preFooter: String =
    """
      |
      |\bibliographystyle{BIBSTYLE} \renewcommand{\refname}{Bibliography}
      |\addcontentsline{toc}{section}{Bibliography}
      |\bibliography{AMbib}
      |\end{document}
    """.stripMargin.replaceAll("AMbib",specs.getBibfile).replaceAll("BIBSTYLE", specs.getBibstyle)

  val btParser: BibTeXParser= new BibTeXParser()
  val btDB: BibTeXDatabase = btParser.parse(fr)

  val entries : Map[Key, BibTeXEntry] = ( btDB getEntries ) asScala

  def matches_regex(x:String, r:String) : Boolean = r.r.findFirstIn(x) match {
    case Some(_) => true
    case None => false
  }

  def matches_one_of_regex(x:String, rs: java.util.ArrayList[String]): Boolean =
    (false /: rs.asScala) ( _ || matches_regex(x, _))

  def runInMyDir(command: Seq[String]): Boolean = {
    sys.process.Process(command, new java.io.File(specs.getAutodir)).! == 0
  }

  val oldarXiv = new scala.util.matching.Regex("""\D*(\d\d)(\d\d)(\d\d\d)\D*""", "year", "month", "number")
  val newarXiv = new scala.util.matching.Regex("""\D*(\d\d)(\d\d)\.(\d\d\d\d)\D*""", "year", "month", "number")

  def comp(k1: Key, k2: Key): Boolean = {
    val e1: BibTeXEntry = entries(k1)
    val e2: BibTeXEntry = entries(k2)
//    println(e1.getField(new Key("title")).toUserString)
//    println(e2.getField(new Key("title")).toUserString)
    def decimal(mold: Option[scala.util.matching.Regex.Match], mnew: Option[scala.util.matching.Regex.Match]): Long =
      (mold, mnew) match {
        case (None, None) => 0 //TODO this happens; why?
        case (Some(m), None) =>
          10 * ((100000 * (m group "year").toInt) + (1000 * (m group "month").toInt) + (m group "number").toInt )
        case (None, Some(m)) =>
          (1000000 * (m group "year").toInt) + (10000 * (m group "month").toInt) + (m group "number").toInt
        case (Some(ma), Some(mb)) =>
          throw new Exception("both old and new style")
      }
    val fields1: util.Map[Key, Value] = e1.getFields
    val fields2: util.Map[Key, Value] = e2.getFields
    val dec1: Long = if (fields1.keySet().contains(new Key("eprint"))) {
      val pp1: String = e1.getField(new Key("eprint")).toUserString
      val m1old  = oldarXiv findFirstMatchIn(pp1)
      val m1new  = newarXiv findFirstMatchIn(pp1)
      decimal(m1old,m1new)
    } else 0
    val dec2: Long = if (fields2.keySet().contains(new Key("eprint"))) {
      val pp2: String = e2.getField(new Key("eprint")).toUserString
      val m2old  = oldarXiv findFirstMatchIn(pp2)
      val m2new  = newarXiv findFirstMatchIn(pp2)
      decimal(m2old,m2new)
    } else 0
    dec1 < dec2
  }

  def main(args: Array[String]) = {
    fr.close()
    var references: String = ""
    for ( a <- entries.keys.toList.sortWith(comp) ) {
      val ttl : String = entries(a).getField(new Key("title")).toUserString
      if ( matches_one_of_regex(ttl, specs.getRegex) ) {
        println(ttl)
        println("---")
        references = references.concat(
        """ \cite{""" + a + """} """
        )
      }
    }
    val fwPre: FileWriter = new FileWriter(new File(fPre))
    fwPre.write(preHeader + references + preFooter)
    print(preHeader + references + preFooter)
    fwPre.close()
    val preLaTeXFirstRun: Boolean  = runInMyDir(Seq("pdflatex", fPreNoExt))
    println("------------------------------------------------------------")
    println("----------- About to run BibTeX ----------------------------")
    println("------------------------------------------------------------")
    val preBibTeXRun: Boolean      = runInMyDir(Seq("bibtex", fPreNoExt))
    println("------------------------------------------------------------")
    println("----------- Finished running BibTeX ------------------------")
    println("------------------------------------------------------------")
    val preLaTeXSecondRun: Boolean = runInMyDir(Seq("pdflatex", fPreNoExt))
    val preLaTeXThirdRun: Boolean  = runInMyDir(Seq("pdflatex", fPreNoExt))
    val bbl = scala.io.Source.fromFile(fPreNoExt + ".bbl").mkString
    val fw: FileWriter = new FileWriter(new File(fNoExt + ".tex"))
    fw.write(
      """
        |\documentclass[12pt]{article}
        |\begin{document}
        |
      """.stripMargin.concat(bbl).concat("""
        |\end{document}
      """.stripMargin)
    )
    fw.close()
    val firstRun:  Boolean = runInMyDir(Seq("pdflatex", fNoExt))
    val secondRun: Boolean = runInMyDir(Seq("pdflatex", fNoExt))
    println(preLaTeXFirstRun)
    println(preLaTeXSecondRun)
    println(preLaTeXThirdRun)
    println(preBibTeXRun)
    println(firstRun)
    println(secondRun)
  }

}
