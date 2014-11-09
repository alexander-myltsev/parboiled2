package org.parboiled2.fast.implementations

import org.parboiled2.fast.ruleprocessing.{ RuleCombiner, RulesTransformer, RulesInliner, ParseRules }
import org.parboiled2.fast.parsers.BaseParsersImpl

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object FastParser {
  def FastParser(rules: ⇒ Unit): FinalFastParserImpl = macro FastParsersImpl.FastParser
}

class FastParsersImpl(val c: whitebox.Context) extends BaseImpl
    with RulesTransformer with RulesInliner
    with ParseRules with BaseParsersImpl
    with RuleCombiner {
  override def FastParser(rules: c.Tree) = super.FastParser(rules)
}
