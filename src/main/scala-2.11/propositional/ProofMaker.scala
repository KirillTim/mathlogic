package propositional

import propositional.ExprTypes.Var
import propositional.Types.{NotTrue, ErrorReason, Proof}

class ProofMaker {
  def apply(fileName: String): Either[ErrorReason, Proof] = {
    null
  }

  def apply(expr: Expr): Either[ErrorReason, List[Expr]] = {
     whenFalse(expr) match {
       case Some(q:Map[Var, Boolean]) => Left(new NotTrue(varsToString(q)))
       case _ => Right(makeProof(expr))
     }
    null
  }

  def makeProof(expr: Expr) = {
    null
  }

  /**
    *
    * @param expr
    * @return Option[Map]
    */
  def whenFalse(expr: Expr) = {
    val vars = expr.getVars
    gen(vars.size).map(x => vars.zip(x).toMap).map(x => if (!expr.evaluate(x)) Some(x) else None).find {
      case Some(_) => true
      case _ => false
    }.map {
      case Some(q) => q
      case _ => None
    }
  }

  private def varsToString(m: Map[Var, Boolean]):String = {
    var q = ""
    m.keys.foreach((x: Var) => q += (x + "=" + (if (m.get(x).get) "И" else "Л") + ", "))
    q.take(q.length - 2)
  }

  private def gen(i: Int): List[List[Boolean]] = i match {
    case 1 => List(List(false), List(true))
    case _ => gen(i - 1).flatMap(x => List(x :+ false, x :+ true))
  }

}
