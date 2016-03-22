package propositional

import propositional.ExprTypes.Term

abstract class Expr(val opPriority: Int) {

  import ExprTypes._

  //change x to phi in this
  def isFreeForSubstitution(x: Term, phi: Expr, affectedVars: Set[Term] = Set()): Boolean = this match {
    case FA(v, expr) if v == x => true
    case FA(v, expr) => expr.isFreeForSubstitution(x, phi, affectedVars + v)
    case EX(v, expr) if v == x => true
    case EX(v, expr) => expr.isFreeForSubstitution(x, phi, affectedVars + v)
    case t@Term(name) if t == x => phi.getVars.forall(p => !affectedVars.contains(p))
    case Term(name, args@_*) => args.forall((p:Term) => p != x || phi.getVars.forall(p => !affectedVars.contains(p)))
    case Predicate(name, args@_*) => args.forall((p:Term) => p != x || phi.getVars.forall(p => !affectedVars.contains(p)))
    case ->(a, b) => a.isFreeForSubstitution(x, phi, affectedVars) && b.isFreeForSubstitution(x, phi, affectedVars)
    case &(a, b) => a.isFreeForSubstitution(x, phi, affectedVars) && b.isFreeForSubstitution(x, phi, affectedVars)
    case V(a, b) => a.isFreeForSubstitution(x, phi, affectedVars) && b.isFreeForSubstitution(x, phi, affectedVars)
    case !!(a) => a.isFreeForSubstitution(x, phi, affectedVars)
    case _ => true
  }

  def varEntersFree(phi: Term): Boolean = this match {
    case FA(v, expr) if v == phi => false
    case FA(v, expr) => expr.varEntersFree(phi)
    case EX(v, expr) if v == phi => false
    case EX(v, expr) => expr.varEntersFree(phi)
    case t@Term(name) => t == phi
    case Term(name, args@_*) => args.exists(p => p.varEntersFree(phi))
    case Predicate(name, args@_*) => args.exists(p => p.varEntersFree(phi))
    case ->(a, b) => a.varEntersFree(phi) || b.varEntersFree(phi)
    case &(a, b) => a.varEntersFree(phi) || b.varEntersFree(phi)
    case V(a, b) => a.varEntersFree(phi) || b.varEntersFree(phi)
    case !!(a) => a.varEntersFree(phi)
    case _ => true
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

  /*def findChanges(x:Term, other: Expr): Option[Set[Expr]] = (this, other) match {
    case (FA(v1, e1), FA(v2, e2)) =>
      if (v1 == x) Some(Set()) else e1.findChanges(x, e2)
    case (EX(v1, e1), EX(v2, e2)) =>
      if (v1 == x) Some(Set()) else e1.findChanges(x, e2)
    case (t1@Term(n1), expr) => //TODO: confirm
      if (t1 == x) Some(Set(expr)) else Some(Set())
    case (Term(n1, args1@_*), Term(n2, args2@_*)) if n1 == n2 =>
      Some(args1.zip(args2).flatMap((p) => p._1.findChanges(x, p._2).getOrElse(Set())).toSet)
    case (Predicate(n1, args1@_*), Predicate(n2, args2@_*)) if n1 == n2 =>
      Some(args1.zip(args2).flatMap((p) => p._1.findChanges(x, p._2).getOrElse(Set())).toSet)
    case (->(a1, b1), ->(a2, b2)) => Some(a1.findChanges(x, a2).getOrElse(Set()) ++ b1.findChanges(x, b2).getOrElse(Set()))
    case (&(a1, b1), &(a2, b2)) => Some(a1.findChanges(x, a2).getOrElse(Set()) ++ b1.findChanges(x, b2).getOrElse(Set()))
    case (V(a1, b1), V(a2, b2)) => Some(a1.findChanges(x, a2).getOrElse(Set()) ++ b1.findChanges(x, b2).getOrElse(Set()))
    case (!!(a1), !!(a2)) => Some(a1.findChanges(x, a2).getOrElse(Set()))
    case _ => None
  }*/

  def concat(a: Option[Set[Expr]], b: Option[Set[Expr]]) : Option[Set[Expr]] = (a,b) match {
    case (None, Some(_)) => None
    case (Some(_), None) => None
    case (None, None) => None
    case (Some(l1), Some(l2)) => Some(l1 ++ l2)
  }

  def findChanges(x: Term, other: Expr) :Option[Set[Expr]] = (this, other) match {
    case (FA(v1, e1), FA(v2, e2)) =>
      if (v1 != v2) None else e1.findChanges(x, e2)
    case (EX(v1, e1), EX(v2, e2)) =>
      if (v1 != v2) None else e1.findChanges(x, e2)
    case (t1@Term(n1), expr) if t1 == x =>
      Some(Set(expr))  //TODO: confirm
    case (Term(n1), Term(n2)) =>
      Some(Set())
    case (Term(n1, args1@_*), Term(n2, args2@_*)) if n1 == n2 =>
      args1.zip(args2).map((p) => p._1.findChanges(x, p._2)).reduce[Option[Set[Expr]]]((a1,a2) => concat(a1,a2))
    case (Predicate(n1, args1@_*), Predicate(n2, args2@_*)) if n1 == n2 =>
      args1.zip(args2).map((p) => p._1.findChanges(x, p._2)).reduce[Option[Set[Expr]]]((a1,a2) => concat(a1,a2))
    case (->(a1, b1), ->(a2, b2)) =>
      concat(a1.findChanges(x, a2), b1.findChanges(x, b2))
    case (&(a1, b1), &(a2, b2)) =>
      concat(a1.findChanges(x, a2), b1.findChanges(x, b2))
    case (V(a1, b1), V(a2, b2)) =>
      concat(a1.findChanges(x, a2), b1.findChanges(x, b2))
    case (!!(a1), !!(a2)) =>
      a1.findChanges(x, a2)
    case _ => None

  }

  def isSubstituted(x: Term, other: Expr): Boolean = {
    val changes = findChanges(x, other)
    changes match {
      case None => false //not the same trees
      case Some(set) =>
        set.size match {
          case 0 => true
          case 1 =>
            isFreeForSubstitution(x, set.head) && other == substituteFree(x, set.head)
          case _ => false
        }
    }
  }

  def substituteFree(what:Term, to:Expr) : Expr = this match {
    case FA(x, b) if x == what => new FA(x, b)
    case FA(x, b) => new FA(x, b.substituteFree(what, to))
    case EX(x, b) if x == what => new EX(x, b)
    case EX(x, b) => new EX(x, b.substituteFree(what, to))
    case t@Term(_) if t == what => to
    case t@Term(_) => t
    //TODO: how to cast to Term* ?
    case Term(name, args@_*) => new Term(name, args.map(_.substituteFree(what, to)).map({ case t: Term => t }): _*)
    case Predicate(name, args@_*) => new Predicate(name, args.map(_.substituteFree(what, to)).map({ case t: Term => t }): _*)
    case ->(a, b) => new ->(a.substituteFree(what, to), b.substituteFree(what, to))
    case &(a, b) => new ExprTypes.&(a.substituteFree(what, to), b.substituteFree(what, to))
    case V(a, b) => new ExprTypes.V(a.substituteFree(what, to), b.substituteFree(what, to))
    case !!(a) => new !!(a.substituteFree(what, to))
  }

  def evaluate(m: Map[String, Boolean]): Boolean //= false

  def getVars: List[Term] //= List()

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

    def needBraces = expr match {
      case t@Term(name, _) if !t.commonPredicates.contains(name) => false
      case _ => true;
    }
  }

  case class FA(varName: Term, expr: Expr) extends Quantifier(varName, expr) {
    override def toString: String = "@" + varName + (if (needBraces) "("+expr+")" else expr)

    override def hashCode = (varName.hashCode * expr.hashCode()) ^ 90533
  }

  case class EX(varName: Term, expr: Expr) extends Quantifier(varName, expr) {
    override def toString: String = "?" + varName + (if (needBraces) "("+expr+")" else expr)

    override def hashCode = (varName.hashCode * expr.hashCode()) ^ 90529
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
        args(0) + "" + name + "" + args(1)
      //else if (name == "'") "("+args(0)+")" + "'"
      else if (name =="'") {
        (args(0) match {
          case Term(name) => args(0)
          case _ => "("+args(0)+")"
        }) + "'"
      }
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
        args(0) + "" + name + "" + args(1)
      //else if (name == "'") "("+args(0)+")" + "'"
      else if (name =="'") {
        (args(0) match {
          case Term(name) => args(0)
          case _ => "("+args(0)+")"
        }) + "'"
      }
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