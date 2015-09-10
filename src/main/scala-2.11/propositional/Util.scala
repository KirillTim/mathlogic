package propositional

import ExprTypes._

object Util {
  def axiomNumber(expr: Expr): Int = expr match {
    case a -> (_ -> b) if a == b => 1
    case (a -> b) -> ((c -> (d -> e)) -> (f -> g)) if a == c && b == d && a == f && e == g => 2
    case a -> (b -> (c & d)) if a == c && b == d => 3
    case (a & _) -> b if a == b => 4
    case (_ & a) -> b if a == b => 5
    case a -> (b V _) if a == b => 6
    case a -> (_ V b) if a == b => 7
    case (a -> b) -> ((c -> d) -> ((e V f) -> g)) if a == e && b == d && b == g && c == f => 8
    case (a -> b) -> ((c -> !!(d)) -> !!(e)) if a == c && a == e && b == d => 9
    case !!(!!(a)) -> b if a == b => 10
    case _ => -1
  }

}
