package org.parboiled2.fast.ruleprocessing

import org.parboiled2.fast.tools.TreeTools

/**
 * Created by Eric on 23.04.14.
 * Inline rule calls.
 *
 * def rule1 = a ~ rule2
 * def rule2 = b ~ c
 *
 * becomes
 *
 * def rule1 = a ~ b ~ c
 * def rule2 = b ~ c
 *
 * If it cannot be inlined (recursive rules) then the rule will be simply called
 */
trait RulesInliner extends RulesTransformer {
  self: TreeTools ⇒
}