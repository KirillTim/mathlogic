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
    case ->(FA(x, e), phi) if e.findChanges(x, phi).getOrElse(Set()).size == 1 =>
      if (e.isSubstituted(x, phi))
        Right(Some(Axiom(11)))
      else
        Left(NotFreeForSubstitution(e.findChanges(x,phi).get.head, x, e, line))
    case ->(phi, EX(x, e)) if e.findChanges(x, phi).getOrElse(Set()).size == 1 =>
      if (e.isSubstituted(x, phi))
        Right(Some(Axiom(12)))
      else
        Left(NotFreeForSubstitution(e.findChanges(x,phi).get.head, x, e, line))
    /*case ->(FA(x, e), phi) if (substitution(e, phi) match {
      case Some(y) => !e.isFreeForSubstitution(y, x)
      case None => false
    }) => Left(NotFreeForSubstitution(substitution(e, phi).get, x, e, line))
    case ->(FA(x, e), phi) if e.isSubstituted(phi) => Right(Some(Axiom(11)))*/
    /*case ->(phi, EX(x, e)) if (substitution(e, phi) match {
      case Some(y) => !e.isFreeForSubstitution(y, x)
      case None => false
    }) => Left(NotFreeForSubstitution(substitution(e, phi).get, x, e, line))
    case ->(phi, EX(x, e)) if phi.isSubstituted(e) => Right(Some(Axiom(12)))*/
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

  /*def substitution(e: Expr, d: Expr): Option[Expr] = e.getChanges(d) match {
    case Some(set) if set.size == 1 =>
      set.find(_ => true).filter({ case (ex, t) => e.substitute(Map(t.name -> ex)) == d }).map(_._1)
    case None => None
    case _ => None
  }*/
}
