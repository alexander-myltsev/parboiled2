package org.parboiled2.fast.ruleprocessing

import scala.collection.mutable

/**
 * Combine rules
 */
trait ReduceRules extends RulesProcessing {
  def combine(rules: mutable.HashMap[String, RuleInfo]): c.Tree
}
