package org.parboiled2.examples
package js

import org.parboiled2._

import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success}

@JSExport
object ABCParserJS {
  @JSExport
  def parse(input: String): String = {
    val parser = new ABCParser(input)
    parser.InputLine.run() match {
      case Success(_)             ⇒ "Expression is valid"
      case Failure(e: ParseError) ⇒ "Expression is not valid: " + parser.formatError(e)
      case Failure(e)             ⇒ "Unexpected error during parsing run: " + e
    }
  }
}
