import scala.annotation.tailrec
import scala.util.{ Success, Failure }
import scala.io.StdIn
import org.parboiled2._

object DSparser {
  var jobDump: String = ""

  def read(s: String) = {
    s match {
      case "" =>
      case line =>
        val parser = new DSparser(s)
        parser.InputLine.run() match {
          case Success(exprAst) => eval(exprAst)
          case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e));
          case Failure(e) => println("Unexpected error during parsing run: " + e);
        }
    }
  }

  def eval(p: DSjobs) =
    p match {
      case analizer: jobAnalizator => println("SRABOTALO!")
      case _ =>
    }

  sealed trait DSjobs

  case class jobAnalizator(var value: Seq[jobLog]) extends DSjobs

  case class oneJobAll(var items: Seq[jobLog]) extends DSjobs

  case class jobLog(value: name_seq) extends DSjobs

  case class name_seq(value: String) extends DSjobs { override def toString = value }

}

class DSparser(val input: ParserInput) extends Parser {

  import DSparser._

  def InputLine: Rule1[jobAnalizator] = rule { oneJobAllRule ~ EOI ~> ((jobList: oneJobAll) => jobAnalizator(jobList.items)) }

  def oneJobAllRule = rule { zeroOrMore(jobLogRule ~ '-') ~> oneJobAll }

  def jobLogRule = rule { checkcheck ~> jobLog }

  def checkcheck = rule { capture(charsRule | '_') ~> name_seq }

  def charsRule = rule { oneOrMore(CharPredicate.Alpha | '_') }
}