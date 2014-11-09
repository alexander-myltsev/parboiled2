package org.parboiled2.fast.parsers

trait BaseParsersImpl extends ParserImplBase {
  import c.universe._

  override def expand(tree: c.Tree) = tree match {
    case q"$lhs.~[$a, $b]($rhs)($c, $d)"        ⇒ parseSeq(lhs, rhs)
    case q"$lhs.|[$a, $b]($rhs)"                ⇒ parseFirst(lhs, rhs)
    case q"$a.this.ch($c)"                      ⇒ parseCh(c)
    case q"$a.this.str($s)"                     ⇒ parseStr(s)
    //    case q"$a.this.valueMap[$b]($m)($hl)"                  ⇒ MapMatch(m)
    //    case q"$a.this.ignoreCase($t)"                         ⇒ IgnoreCase(t)
    //    case q"$a.this.predicate($p)"                          ⇒ CharPredicateMatch(p)
    //    case q"$a.this.anyOf($s)"                              ⇒ AnyOf(s)
    //    case q"$a.this.noneOf($s)"                             ⇒ NoneOf(s)
    //    case q"$a.this.ANY"                                    ⇒ ANY
    case q"$a.this.optional[$b, $c]($arg)($o)"  ⇒ parseOptional(arg)
    //    case q"$a.this.zeroOrMore[$b, $c]($arg)($s)"           ⇒ ZeroOrMore(OpTree(arg), collector(s))
    case q"$a.this.oneOrMore[$b, $c]($arg)($s)" ⇒ parseOneOrMore(arg)
    //    case q"$base.times[$a, $b]($r)($s)"                    ⇒ Times(base, OpTree(r), collector(s))
    case q"$a.this.&($arg)"                     ⇒ parseAndPredicate(arg)
    case q"$a.unary_!()"                        ⇒ parseNotPredicate(a)
    //    case q"$a.this.test($flag)"                            ⇒ SemanticPredicate(flag)
    //    case q"$a.this.capture[$b, $c]($arg)($d)"              ⇒ Capture(OpTree(arg))
    //    case q"$a.this.run[$b]($arg)($c.fromAux[$d, $e]($rr))" ⇒ RunAction(arg, rr)
    //    case q"$a.this.push[$b]($arg)($hl)"                    ⇒ PushAction(arg, hl)
    //    case q"$a.this.drop[$b]($hl)"                          ⇒ DropAction(hl)
    //    case q"$a.this.runSubParser[$b, $c]($f)"               ⇒ RunSubParser(f)
    //    case q"$a.named($name)"                                ⇒ Named(OpTree(a), name)
    //    case x @ q"$a.this.str2CharRangeSupport($l).-($r)"     ⇒ CharRange(l, r)
    //    case q"$a.this.charAndValue[$t]($b.ArrowAssoc[$t1]($c).->[$t2]($v))($hl)" ⇒
    //      Sequence(CharMatch(c), PushAction(v, hl))
    //    case q"$a.this.stringAndValue[$t]($b.ArrowAssoc[$t1]($s).->[$t2]($v))($hl)" ⇒
    //      Sequence(StringMatch(s), PushAction(v, hl))
    //    case q"$a.this.rule2ActionOperator[$b1, $b2]($r)($o).~>.apply[..$e]($f)($g, support.this.FCapture.apply[$ts])" ⇒
    //      Sequence(OpTree(r), Action(f, ts))
    //    case x @ q"$a.this.rule2WithSeparatedBy[$b1, $b2]($base.$fun[$d, $e]($arg)($s)).separatedBy($sep)" ⇒
    //      val (op, coll, separator) = (OpTree(arg), collector(s), Separator(OpTree(sep)))
    //      fun.decodedName.toString match {
    //        case "zeroOrMore" ⇒ ZeroOrMore(op, coll, separator)
    //        case "oneOrMore"  ⇒ OneOrMore(op, coll, separator)
    //        case "times"      ⇒ Times(base, op, coll, separator)
    //        case _            ⇒ c.abort(x.pos, "Unexpected Repeated fun: " + fun)
    //      }
    //    case call @ (Apply(_, _) | Select(_, _) | Ident(_)) ⇒ parseRuleCall(call)
    case _                                      ⇒ super.expand(tree)
  }

  private def parseStr(str: c.Tree) = q"""
    var ix = 0
    while (ix < $str.length && $str.charAt(ix) == cursorChar()) {
      ix += 1
      advance()
    }
    ix == $str.length
  """

  private def parseCh(ch: c.Tree) = q"""
    if ($ch == cursorChar()) {
      advance()
      true
    } else {
      false
    }
  """

  private def parseSeq(lhs: c.Tree, rhs: c.Tree) = q"""
    ${expand(lhs)} && ${expand(rhs)}
  """

  private def parseFirst(lhs: c.Tree, rhs: c.Tree) = q"""
    val mark = _cursor
    if (${expand(lhs)}) {
      true
    } else {
      _cursor = mark
      ${expand(rhs)}
    }
  """

  private def parseOptional(arg: c.Tree) = q"""
    val mark = _cursor
    if (!${expand(arg)}) {
      _cursor = mark
    }
    true
  """

  private def parseOneOrMore(arg: c.Tree) = q"""
    var mark = _cursor
    if (${expand(arg)}) {
      while (${expand(arg)}) {
        mark = _cursor
      }
      _cursor = mark
      true
    } else {
      _cursor = mark
      false
    }
  """

  private def parseNotPredicate(arg: c.Tree) = q"""
    val mark = _cursor
    val res = ${expand(arg)}
    _cursor = mark
    !res
  """

  private def parseAndPredicate(arg: c.Tree) = q"""
    val mark = _cursor
    val res = ${expand(arg)}
    _cursor = mark
    res
  """
}
