package org.parboiled2.fast.ruleprocessing

import scala.collection.mutable

/**
 * Transform rules
 */
trait MapRules extends RulesProcessing {
  def process(rules: mutable.HashMap[String, RuleInfo]): mutable.HashMap[String, RuleInfo] = rules
}