package propositional

import ExprTypes._
import _root_.mathlogic.propositionalCalculus.Axiom
import _root_.mathlogic.propositionalCalculus.Forall
import _root_.mathlogic.propositionalCalculus.Implication
import propositional.Types.Axiom

object Util {
  def axiomNumber(expr: Expr): Option[Axiom] = expr match {
    case a -> (_ -> b) if a == b => Some(Axiom(1))
    case (a -> b) -> ((c -> (d -> e)) -> (f -> g)) if a == c && b == d && a == f && e == g => Some(Axiom(2))
    case a -> (b -> (c & d)) if a == c && b == d => Some(Axiom(3))
    case (a & _) -> b if a == b => Some(Axiom(4))
    case (_ & a) -> b if a == b => Some(Axiom(5))
    case a -> (b V _) if a == b => Some(Axiom(6))
    case a -> (_ V b) if a == b => Some(Axiom(7))
    case (a -> b) -> ((c -> d) -> ((e V f) -> g)) if a == e && b == d && b == g && c == f => Some(Axiom(8))
    case (a -> b) -> ((c -> !!(d)) -> !!(e)) if a == c && a == e && b == d => Some(Axiom(9))
    case !!(!!(a)) -> b if a == b => Some(Axiom(10))
    case ->(FA(x, e), phi) if e.isSubstituted(phi) => Some(Axiom(11))
    case ->(phi, EX(x, e)) if phi.isSubstituted(e) => Some(Axiom(12))
    case _ => None
  }
}
