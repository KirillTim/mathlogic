package propositional

import ExprTypes._
import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {

  def MainRule = rule {
    expression ~ EOI
  }

  private def expression: Rule1[Expr] = rule {
    oneOrMore(disjunction).separatedBy("->") ~>
      ((a: Seq[Expr]) => {
        a.reduceRight(->)
      })
  }

  private def disjunction: Rule1[Expr] = rule {
    oneOrMore(conjunction).separatedBy("|") ~>
      ((a: Seq[Expr]) => {
        a.reduceLeft(V)
      })
  }

  private def conjunction: Rule1[Expr] = rule {
    oneOrMore(unary).separatedBy("&") ~>
      ((a: Seq[Expr]) => {
        a.reduceLeft(ExprTypes.&)
      })
  }

  private def unary: Rule1[Expr] = rule {
    negate | variable | parens
  }

  private def negate: Rule1[Expr] = rule {
    "!" ~ unary ~> ((a: Expr) => {
      new !!(a)
    })
  }

  private def parens: Rule1[Expr] = rule {
    "(" ~ expression ~ ")"
  }

  private def variable: Rule1[Expr] = rule {
    capture(upper) ~> ((a: String) => {
      new Var(a)
    })
  }

  private def upper: Rule0 =
    rule {
      anyOf("PYFGCRLAOEUIDHTNSQJKXBMWVZ") ~ zeroOrMore(anyOf("0123456789"))
    }

  private def lowerE: Rule0 =
    rule {
      anyOf("pyfgcrlaoeuidhtnsqjkxbmwvz") ~ zeroOrMore(anyOf("0123456789"))
    }

  private def lower: Rule0 =
    rule {
      anyOf("pyfgcrlaoeuidhtnsqjkxbmwvz")
    }

}
