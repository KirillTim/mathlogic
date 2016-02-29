package propositional

import propositional.ExprTypes.Term

abstract class Expr(val opPriority: Int) {

  import ExprTypes._

  def isFreeForSubstitution(e: Expr, x: Term, affectedVars: Set[Term] = Set()): Boolean = this match {
    case FA(y, phi) => phi.isFreeForSubstitution(e, x, affectedVars + y)
    case EX(y, phi) => phi.isFreeForSubstitution(e, x, affectedVars + y)
    case ->(a, b) => a.isFreeForSubstitution(e, x, affectedVars) && b.isFreeForSubstitution(e, x, affectedVars)
    case V(a, b) => a.isFreeForSubstitution(e, x, affectedVars) && b.isFreeForSubstitution(e, x, affectedVars)
    case &(a, b) => a.isFreeForSubstitution(e, x, affectedVars) && b.isFreeForSubstitution(e, x, affectedVars)
    case t@Term(name) if affectedVars.contains(t) && name == x.toString => false
    case Term(name) if name == x.toString => e.getVars.forall(!affectedVars.contains(_))
    case Term(name, b@_*) => b.forall(_.isFreeForSubstitution(e, x, affectedVars))
    case Predicate(name, b@_*) => b.forall(_.isFreeForSubstitution(e, x, affectedVars))
    case _ => true
  }

  def entersFree(e: Term, affectedVars: Set[Term] = Set()): Boolean = {
    this match {
      case FA(x, b) => b.entersFree(e, affectedVars + x)
      case EX(x, b) => b.entersFree(e, affectedVars + x)
      case ->(a, b) => a.entersFree(e, affectedVars) || b.entersFree(e, affectedVars)
      case V(a, b) => a.entersFree(e, affectedVars) || b.entersFree(e, affectedVars)
      case &(a, b) => a.entersFree(e, affectedVars) || b.entersFree(e, affectedVars)
      case !!(a) => a.entersFree(e, affectedVars)
      case t@Term(_) if t == e => !affectedVars.contains(t)
      case Term(name, b@_*) => b.exists(_.entersFree(e, affectedVars))
      case Predicate(name, b@_*) => b.exists(_.entersFree(e, affectedVars))
      case _ => false
    }
  }

  def substitute(variables: Map[String, Expr]): Expr = this match {
    case FA(x, b) => FA(x, b.substitute(variables))
    case EX(x, b) => EX(x, b.substitute(variables))
    case Term(name) if variables.get(name).isDefined => variables.get(name).get
    case v@Term(name) => v
    case &(a, b) => ExprTypes.&(a.substitute(variables), b.substitute(variables))
    case ->(a, b) => ->(a.substitute(variables), b.substitute(variables))
    case V(a, b) => ExprTypes.V(a.substitute(variables), b.substitute(variables))
    case !!(a) => !!(a.substitute(variables))
    case Predicate(name, b@_*) => Predicate(name, b.map(_.substitute(variables)).map({ case t: Term => t }): _*)
    case Term(name, b@_*) => Term(name, b.map(_.substitute(variables)).map({ case t: Term => t }): _*)
  }

  def mergeChanges(mapA: Option[Set[(Expr, Term)]],
                   mapB: Option[Set[(Expr, Term)]]): Option[Set[(Expr, Term)]] = Some(mapA.getOrElse(Set()) ++ mapB.getOrElse(Set()))

  def isSubstituted(e: Expr): Boolean = getChanges(e) match {
    case Some(set) if set.size <= 1 =>
      set.forall(p => isFreeForSubstitution(p._1, p._2) && substitute(Map(p._2.name -> p._1)) == e)
    case _ => false
  }

  def getChanges(e: Expr): Option[Set[(Expr, Term)]] = this match {
    case ->(a, b) => e match {
      case ->(c, d) => mergeChanges(a.getChanges(c), b.getChanges(d))
      case _ => None
    }
    case V(a, b) => e match {
      case V(c, d) => mergeChanges(a.getChanges(c), b.getChanges(d))
      case _ => None
    }
    case &(a, b) => e match {
      case &(c, d) => mergeChanges(a.getChanges(c), b.getChanges(d))
      case _ => None
    }
    case !!(a) => e match {
      case !!(b) => a.getChanges(b)
      case _ => None
    }
    case FA(t, ex) => e match {
      case FA(g, dx) => ex.getChanges(dx)
      case _ => None
    }
    case EX(t, ex) => e match {
      case EX(g, dx) => ex.getChanges(dx)
      case _ => None
    }
    case Predicate(name, b@_*) => e match {
      case Predicate(name2, b2@_*) if name == name2 && b.length == b2.length =>
        if (b.isEmpty) Some(Set())
        else b.zip(b2).map(p => p._1.getChanges(p._2)).foldRight(Option.empty[Set[(Expr, Term)]])(mergeChanges)
      case _ => None
    }
    case t@Term(name) if t != e => Some(Set(e -> t))
    case Term(name, b@_*) => e match {
      case Term(name2, b2@_*) if name == name2 && b.length == b2.length =>
        if (b.isEmpty) Some(Set())
        else b.zip(b2).map(p => p._1.getChanges(p._2)).foldRight(Option.empty[Set[(Expr, Term)]])(mergeChanges)
      case _ => None
    }
    case _ => None
  }

  def evaluate(m: Map[String, Boolean]): Boolean

  def getVars: List[Term]

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

abstract class BinaryExpr(val left: Expr, val right: Expr, val priority: Int, val delim: String) extends Expr(priority) {
  override def getVars: List[Term] = left.getVars ::: right.getVars

  override lazy val toString = str2(left, right, delim)
}

object ExprTypes {

  abstract class Quantifier(varName: Term, expr: Expr) extends Expr(11) {
    override def evaluate(m: Map[String, Boolean]): Boolean = expr.evaluate(m)

    override def getVars = expr.getVars

  }

  case class FA(varName: Term, expr: Expr) extends Quantifier(varName, expr) {
    override def toString: String = "@" + varName + expr

    override lazy val hashCode = (varName.hashCode * expr.hashCode()) ^ 90533
  }

  case class EX(varName: Term, expr: Expr) extends Quantifier(varName, expr) {
    override def toString: String = "?" + varName + expr

    override lazy val hashCode = (varName.hashCode * expr.hashCode()) ^ 90529
  }

  case class V(a: Expr, b: Expr) extends BinaryExpr(a, b, 9, "|") {
    override def evaluate(m: Map[String, Boolean]): Boolean = a.evaluate(m) || b.evaluate(m)

    override lazy val hashCode = (a.hashCode * 12569) ^ (b.hashCode * 257)
  }

  type Conj = &

  case class &(a: Expr, b: Expr) extends BinaryExpr(a, b, 10, "&") {
    override def evaluate(m: Map[String, Boolean]): Boolean = a.evaluate(m) && b.evaluate(m)

    override lazy val hashCode = (a.hashCode * 8647) ^ (b.hashCode * 257)
  }

  case class Const(x: Boolean) extends Expr(20) {
    override def evaluate(m: Map[String, Boolean]): Boolean = x

    override def getVars: List[Term] = List()

    override def toString: String = x.toString

    override lazy val hashCode = if (x) 1231 else 1237
  }

  case class Term(name: String, args: Term*) extends Expr(20) {
    val commonPredicates = Seq("=", "*", "+")

    override def evaluate(m: Map[String, Boolean]): Boolean = {
      if (args.nonEmpty)
        throw new IllegalArgumentException("Not a Variable: " + toString)

      if (!(m contains name))
        throw new IllegalArgumentException("Can't find value for " + name)
      else
        (m get name).get
    }

    override def getVars: List[Term] = {
      if (args.nonEmpty)
        args.foldLeft(List[Term]())((l, t) => t.getVars ::: l)
      else
        List(this)
    }

    override def toString = {
      if (args.length == 2 && commonPredicates.contains(name))
        args(0) + " " + name + " " + args(1)
      else if (name == "'") args(0) + "'"
      else if (args.isEmpty) name
      else name + "(" + args.mkString(",") + ")"
    }

    override lazy val hashCode = "Term".hashCode ^ name.hashCode + args.mkString.hashCode
  }

  case class Predicate(name: String, args: Term*) extends Expr(20) {
    val commonPredicates = Seq("=", "*", "+")

    override def evaluate(m: Map[String, Boolean]): Boolean = {
      throw new UnsupportedOperationException("Can't evaluate predicate: " + toString)
    }

    override def getVars: List[Term] = args.foldLeft(List[Term]())((l, t) => t.getVars ::: l)

    override def toString = {
      if (args.length == 2 && commonPredicates.contains(name))
        args(0) + " " + name + " " + args(1)
      else if (name == "'") args(0) + "'"
      else if (args.isEmpty) name
      else name + "(" + args.mkString(",") + ")"
    }

    override lazy val hashCode = "Predicate".hashCode ^ name.hashCode + args.mkString.hashCode
  }

  type Impl = ->

  case class ->(var a: Expr, var b: Expr) extends BinaryExpr(a, b, 8, "->") {
    override def evaluate(m: Map[String, Boolean]): Boolean = (!a.evaluate(m)) || b.evaluate(m)

    override lazy val hashCode = (a.hashCode * 15137) ^ (b.hashCode * 257)
  }

  type Not = !!

  case class !!(var a: Expr) extends Expr(11) {
    override def evaluate(m: Map[String, Boolean]): Boolean = !a.evaluate(m)

    override def getVars = a.getVars

    override lazy val toString: String = a match {
      case v: Term => "!" + a.toString
      case v: Const => "!" + a.toString
      case v: Not => "!" + a.toString
      case _ => "!(" + a.toString + ")"
    }

    override lazy val hashCode = (a.hashCode ^ 29611) * 40241
  }

}