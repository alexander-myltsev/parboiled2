package org.parboiled2.fast.ruleprocessing

import scala.reflect.macros.whitebox

trait RulesProcessing {
  val c: whitebox.Context
  type RuleType = Seq[c.Type]
  type RuleCode = c.Tree
  type ParamInfo = (c.TermName, c.Type)

  case class RuleInfo(typ: RuleType, code: RuleCode, params: List[c.Tree], typeParams: List[c.universe.TypeDef], oldCode: c.Tree) {
    override def toString =
      s"""RuleInfo {
         |\t typ:        $typ
         |\t code:       $code
         |\t params:     $params
         |\t typeParams: $typeParams
         |\t oldCode:    $oldCode
       """.stripMargin.replace("fastparsers.framework.implementations.FastParsers", "fp")
  }
}
