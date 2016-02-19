package propositional

import scala.collection.mutable

abstract class Expr(val opPriority: Int) {

  import ExprTypes._

  def evaluate(m: Map[String, Boolean]): Boolean

  def getVars: mutable.HashSet[String]

  protected def str2(a: Expr, b: Expr, delim: String): String = {
    var aStr = a.toString
    var bStr = b.toString
    if (this.opPriority >= a.opPriority)
      aStr = "(" + aStr + ")"
    if (this.opPriority >= b.opPriority)
      bStr = "(" + bStr + ")"
    aStr + delim + bStr
  }

  override def equals(other: Any): Boolean = {
    other.getClass == this.getClass && other.hashCode() == this.hashCode()
  }

  def ->:(other: Expr): -> = new ->(other, this)

  def V(other: Expr): V = new V(this, other)

  def &(other: Expr): & = new &(this, other)

  def !!(other: Expr): !! = new !!(this)
}

abstract class BinaryExpr(val left: Expr, val right: Expr, val priority: Int, val delim:String) extends Expr(priority) {
  override lazy val getVars: mutable.HashSet[String] = left.getVars ++= right.getVars

  override lazy val toString = str2(left, right, delim)
}

object ExprTypes {

  /*abstract class Quantifier(varName: Var, expr: Expr) extends Expr(11) {
    override def evaluate(m: Map[String, Boolean]): Boolean = expr.evaluate(m)

    override def getVars: mutable.HashSet[String] = expr.getVars

  }

  case class FA(varName: Var, expr: Expr) extends Quantifier(varName, expr) {
    override def toString: String = "@" + varName + expr

    override lazy val hashCode = (varName.hashCode * expr.hashCode()) ^ 90533
  }

  case class EX(varName: Var, expr: Expr) extends Quantifier(varName, expr) {
    override def toString: String = "?" + varName + expr

    override lazy val hashCode = (varName.hashCode * expr.hashCode()) ^ 90529
  }*/

  type Disj = V

  case class V(val a: Expr, val b: Expr) extends BinaryExpr(a, b, 9, "|") {
    override def evaluate(m: Map[String, Boolean]): Boolean = a.evaluate(m) || b.evaluate(m)

    override lazy val hashCode = (a.hashCode * 12569) ^ (b.hashCode * 257)
  }

  type Conj = &

  case class &(val a: Expr, val b: Expr) extends BinaryExpr(a, b, 10, "&") {
    override def evaluate(m: Map[String, Boolean]): Boolean = a.evaluate(m) && b.evaluate(m)

    override lazy val hashCode = (a.hashCode * 8647) ^ (b.hashCode * 257)
  }

  case class Const(x: Boolean) extends Expr(20) {
    override def evaluate(m: Map[String, Boolean]): Boolean = x

    override def getVars: mutable.HashSet[String] = mutable.HashSet.empty[String]

    override def toString: String = x.toString

    override lazy val hashCode = if (x) 1231 else 1237
  }

  case class Term(val name:String, val args:Term*) extends Expr(20) {
    val commonPredicates = Seq("=", "*", "+")

    override def evaluate(m: Map[String, Boolean]): Boolean = {
      if (args.nonEmpty)
        throw new IllegalArgumentException("Not a Variable: " + toString)

      if (!(m contains name))
          throw new IllegalArgumentException("Can't find value for " + name)
      else
        (m get name).get
    }

    override def getVars: mutable.HashSet[String] = {
      if (args.nonEmpty)
        throw new IllegalArgumentException("Not a Variable: " + toString)
      new mutable.HashSet() += name
    }

    override def toString = {
      if (args.length == 2 && commonPredicates.contains(name))
        args(0) + " " + name + " " + args(1)
      else if (name == "'") args(0) + "'"
      else if (args.isEmpty) name
      else name +"(" + args.mkString(",") +")"
    }

    override lazy val hashCode = name.hashCode + args.mkString(",").hashCode

  }

  case class Var(val name: String) extends Expr(20) {

    override def evaluate(m: Map[String, Boolean]): Boolean = {
      if (!(m contains name))
        throw new IllegalArgumentException("Can't find value for " + name)
      else
        (m get name).get
    }

    override def getVars: mutable.HashSet[String] = new mutable.HashSet() += name

    override def toString: String = name

    override lazy val hashCode = name.hashCode
  }

  type Impl = ->

  case class ->(var a: Expr, var b: Expr) extends BinaryExpr(a, b, 8, "->") {
    override def evaluate(m: Map[String, Boolean]): Boolean = (!a.evaluate(m)) || b.evaluate(m)

    override lazy val hashCode = (a.hashCode * 15137) ^ (b.hashCode * 257)
  }

  type Not = !!

  case class !!(var a: Expr) extends Expr(11) {
    override def evaluate(m: Map[String, Boolean]): Boolean = !a.evaluate(m)

    override def getVars: mutable.HashSet[String] = a.getVars

    override lazy val toString: String = a match {
      case v: Term => "!" + a.toString
      case v: Const => "!" + a.toString
      case v: Not => "!" + a.toString
      case _ => "!(" + a.toString + ")"
    }

    override lazy val hashCode = (a.hashCode ^ 29611) * 40241
  }

}