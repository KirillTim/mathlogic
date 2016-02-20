package propositional

import org.parboiled2._
import propositional.ExprTypes._

class ExpressionParser(val input: ParserInput) extends Parser {

  def inputLine = rule {
    expression ~ EOI
  }

  def derivationInputLine: Rule1[(List[Expr], Expr)] = rule {
    ((zeroOrMore(expression).separatedBy(",") ~> ((a: Seq[Expr]) => a.toList)) ~
      "|-" ~
      expression) ~> ((a: List[Expr], b: Expr) => (a, b)) ~
      EOI
  }

  private def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A]
  = rule { a ~ zeroOrMore(divider ~ a ~> b) }

  private def expression: Rule1[Expr] = rule { disjunction ~ zeroOrMore("->" ~ expression ~> ExprTypes.->) }
  private def disjunction: Rule1[Expr] = rule { conjunction ~ zeroOrMore("|" ~ conjunction ~> V) }
  private def conjunction: Rule1[Expr] = rule { unary ~ zeroOrMore("&" ~ unary ~> ExprTypes.&) }

  private def unary: Rule1[Expr] = rule { predicate | negation | parens | ("@" ~ variable ~ unary ~> ((a, b) => FA(a, b))) |
    ("?" ~ variable ~ unary ~> ((a, b) => EX(a, b))) }
  def predicate: Rule1[Expr] = rule { term ~ "=" ~ term ~> ((a: Term, b: Term) => Predicate("=", a, b)) |
    capture(oneOrMore(CharPredicate.UpperAlpha) ~ zeroOrMore(CharPredicate.AlphaNum)) ~ optional("(" ~ zeroOrMore(term).separatedBy(",") ~ ")") ~>
      ((a: String, b:Option[Seq[Term]]) => if (b.isEmpty) Term(a) else Predicate(a, b.get: _*)) | term }
  def term: Rule1[Term] =
    leftAssoc(summable, (a: Term, b: Term) => Term("+", a, b), "+")
  private def summable: Rule1[Term] =
    leftAssoc(mullable, (a: Term, b: Term) => Term("*", a, b), "*")
  private def mullable: Rule1[Term] =
    rule {
      ((capture(CharPredicate.LowerAlpha) ~
        "(" ~
        oneOrMore(term).separatedBy(",") ~
        ")" ~>
        ((a: String, b: Seq[Term]) => Term(a, b: _*))) |
        variable |
        ("(" ~ term ~ ")") |
        (str("0") ~> (() => Term("0")))) ~
        zeroOrMore(capture("'")) ~> ((a: Term, b: Seq[_]) => wrapInQuote(a, b.length)) }

  private def wrapInQuote(e: Term, n: Int): Term = {
    if (n < 1) e else wrapInQuote(Term("'", e), n - 1)
  }

  private def negation: Rule1[Expr] = rule { "!" ~ unary ~> !! }
  private def variable: Rule1[Term] = rule { capture(letters) ~> ((a: String) => Term(a)) }
  private def letters: Rule0 = rule { oneOrMore(CharPredicate.LowerAlpha) ~ zeroOrMore(CharPredicate.Digit) }
  private def parens: Rule1[Expr] = rule { "(" ~ expression ~ ")" }
}


