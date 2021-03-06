package propositional

import ExprTypes._
import propositional.Types.{NotFreeForSubstitution, WrongProof, Axiom}

object Util {
  def axiomNumber(expr: Expr, line: Int): Either[WrongProof, Option[Axiom]] = expr match {
    case a -> (_ -> b) if a == b => Right(Some(Axiom(1)))
    case (a -> b) -> ((c -> (d -> e)) -> (f -> g)) if a == c && b == d && a == f && e == g => Right(Some(Axiom(2)))
    case a -> (b -> (c & d)) if a == c && b == d => Right(Some(Axiom(3)))
    case (a & _) -> b if a == b => Right(Some(Axiom(4)))
    case (_ & a) -> b if a == b => Right(Some(Axiom(5)))
    case a -> (b V _) if a == b => Right(Some(Axiom(6)))
    case a -> (_ V b) if a == b => Right(Some(Axiom(7)))
    case (a -> b) -> ((c -> d) -> ((e V f) -> g)) if a == e && b == d && b == g && c == f => Right(Some(Axiom(8)))
    case (a -> b) -> ((c -> !!(d)) -> !!(e)) if a == c && a == e && b == d => Right(Some(Axiom(9)))
    case !!(!!(a)) -> b if a == b => Right(Some(Axiom(10)))
    case ->(FA(x, e), phi) if e.findChanges(x, phi).isDefined =>
      if (e.isSubstituted(x, phi))
        Right(Some(Axiom(11)))
      else
        Left(NotFreeForSubstitution(e.findChanges(x,phi).get.headOption.getOrElse(x), x, e, line))
    case ->(phi, EX(x, e)) if e.findChanges(x, phi).isDefined =>
      if (e.isSubstituted(x, phi))
        Right(Some(Axiom(12)))
      else
        Left(NotFreeForSubstitution(e.findChanges(x,phi).get.headOption.getOrElse(x), x, e, line))
    case ->(Predicate("=", a, b), Predicate("=", Term("'", c), Term("'", d))) if (a, b) == (c, d) => Right(Some(Axiom(13)))
    case ->(Predicate("=", a, b), ->(Predicate("=", c, d), Predicate("=", e, f)))
      if (a, b) == (c, e) && d == f => Right(Some(Axiom(14)))
    case ->(Predicate("=", Term("'", a), Term("'", b)), Predicate("=", c, d)) if (a, b) == (c, d) => Right(Some(Axiom(15)))
    case !!(Predicate("=", Term("'", a), Term("0"))) => Right(Some(Axiom(16)))
    case Predicate("=", Term("+", a, Term("'", b)), Term("'", Term("+", c, d))) if (a, b) == (c, d) => Right(Some(Axiom(16)))
    case Predicate("=", Term("+", a, Term("0")), b) if a == b => Right(Some(Axiom(17)))
    case Predicate("=", Term("*", a, Term("0"), Term("0"))) => Right(Some(Axiom(18)))
    case Predicate("=", Term("*", a, Term("'", b)), Term("+", Term("*", c, d), e)) if (a, b) == (c, d) && a == e => Right(Some(Axiom(19)))
    case ->(&(phi, FA(x, ->(psi, xi))), theta) if {
      psi == theta && psi.varEntersFree(x) &&
        psi.substitute(Map(x.toString -> Term("0"))) == phi &&
        psi.substitute(Map(x.toString -> Term("'", x))) == xi
    } => Right(Some(Axiom(20)))
    case _ => Right(None)
  }
}
