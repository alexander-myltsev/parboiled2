package org.parboiled2.fast.ruleprocessing

import scala.collection.mutable
import org.parboiled2.fast.tools.TreeTools
import org.parboiled2.fast.parsers.ParserImplBase

/**
 * Create the "final" code for each rule
 */
trait ParseRules extends MapRules {
  self: ParserImplBase with TreeTools ⇒

  import c.universe._
  import c.universe.internal._

  override def process(rules: mutable.HashMap[String, RuleInfo]) = {
    val rulesMap = super.process(rules)

    val map = new mutable.HashMap[String, RuleInfo]()
    for (k ← rulesMap.keys) {
      val rule = rulesMap(k)
      map += ((k, rule.copy(code = createRuleDef(k, rule))))
    }

    //println(map.mkString(">>>>>\n", "\n----\n", "\n====="))
    map
  }

  private def createRuleDef(name: String, rule: RuleInfo): c.Tree = {
    val ruleName = TermName(name)

    q"""def $ruleName[..${rule.typeParams}] = { ${expand(rule.code)} }"""
  }
}
