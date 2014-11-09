package org.parboiled2.fast.parsers

import scala.reflect.macros.whitebox

trait ParserImplBase {
  val c: whitebox.Context

  import c.universe._

  /**
   * Expand a combinator into an imperative code
   * @param tree the combinator to expand
   * @return  The expanded code
   */
  def expand(tree: c.Tree): c.Tree = c.abort(c.enclosingPosition, "Not implemented combinator " + show(tree))
}
