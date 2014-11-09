package org.parboiled2.fast.ruleprocessing

import scala.collection.mutable
import scala.reflect.macros.whitebox

/**
 * Create the final parser object
 */
trait RuleCombiner extends ReduceRules {
  val c: whitebox.Context

  import c.universe._

  def combine(rules: mutable.HashMap[String, RuleInfo]) = {
    val anon = TypeName(c.freshName)
    val dmmy = TermName(c.freshName) //no joke : see http://stackoverflow.com/questions/14370842/getting-a-structural-type-with-an-anonymous-classs-methods-from-a-macro

    val methods = rules.values.map(_.code)

    //println(methods.mkString(">>>>>\n", "\n----\n", "\n====="))

    val methodsEmpty = rules.keySet.map { k ⇒
      val rule = rules(k)
      val ruleName = TermName(k)
      rule.params match {
        case Nil    ⇒ q"""@scala.annotation.compileTimeOnly("can't be used outside of FastParser") def $ruleName[..${rule.typeParams}]: org.parboiled.Rule[..${rule.typ}] = ???"""
        case params ⇒ q"""@scala.annotation.compileTimeOnly("can't be used outside of FastParser") def $ruleName[..${rule.typeParams}](..${rule.params}): org.parboiled.Rule[..${rule.typ}] = ???"""
      }
    }

    //

    val tree = q"""
     class $anon extends org.parboiled2.fast.implementations.FinalFastParserImpl {
         import scala.collection.mutable.ListBuffer
         import scala.reflect.runtime.universe._

         var _cursor: Int = 0
         def cursorChar() =
           if (_cursor == input.length) EOI
           else input charAt _cursor

         def advance(): Boolean =
           if (_cursor <= input.length) {
             _cursor += 1
             true
           } else false

         var input: org.parboiled2.ParserInput = _

         ..$methods
     }
     val $dmmy = 0
     new $anon
   """
    tree
  }
}
