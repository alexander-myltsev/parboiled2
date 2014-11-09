package org.parboiled2.fast.tools

import scala.reflect.macros.whitebox

trait TreeTools {
  val c: whitebox.Context
  import c.universe._
}
