package org.parboiled2.examples
package js

import org.parboiled2._

import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success}

@JSExport
object Calculator2JS {
  @JSExport
  def parse(input: String): String = {
    val parser = new Calculator2(input)
    parser.InputLine.run() match {
      case Success(x)             ⇒ "Expression is valid: " + x + " | " + Calculator2.eval(x)
      case Failure(e: ParseError) ⇒ "Expression is not valid: " + parser.formatError(e)
      case Failure(e)             ⇒ "Unexpected error during parsing run: " + e
    }
  }
}
