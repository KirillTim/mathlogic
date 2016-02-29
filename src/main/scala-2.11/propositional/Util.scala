package propositional

import ExprTypes._
import _root_.mathlogic.propositionalCalculus.Axiom
import _root_.mathlogic.propositionalCalculus.Forall
import _root_.mathlogic.propositionalCalculus.Implication
import propositional.Types.{NotFreeForSubstitution, WrongProof, Axiom}

object Util {
  def axiomNumber(expr: Expr): Either[WrongProof, Option[Axiom]] = expr match {
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
    case ->(FA(x, e), phi) if e.isSubstituted(phi) => Right(Some(Axiom(11)))
    case ->(FA(x, e), phi)  if (substitution(e, phi) match {
        case Some(y) => !e.isFreeForSubstitution(y, x)
        case None => false
      }) => Some(NotFreeForSubstitution(substitution(e, phi).get, x, e, line))
    case ->(phi, EX(x, e)) if phi.isSubstituted(e) => Right(Some(Axiom(12)))
    case _ => Right(None)
  }

  def substitution(e: Expr, d: Expr): Option[Expr] = e.getChanges(d) match {
    case Some(set) if set.size == 1 =>
      set.find(_ => true).filter({ case (ex, t) => e.substitute(Map(t.name -> ex)) == d}).map(_._1)
    case None => None
    case _ => None
  }
}
