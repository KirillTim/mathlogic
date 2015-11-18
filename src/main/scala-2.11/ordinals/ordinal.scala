package ordinals

import scala.BigInt

sealed trait Ordinal

case class *(left: Ordinal, right: Ordinal) extends Ordinal {
  override def toString: String = left.toString+"*"+right.toString
}

case class +(left: Ordinal, right: Ordinal) extends Ordinal {
  override def toString: String = left.toString+"+"+right.toString
}

case class ^(left: Ordinal, right: Ordinal) extends Ordinal {
  override def toString: String = left.toString+"^"+right.toString
}

case class W() extends Ordinal {
  override def toString: String = "w"
}

case class Nat(i: Int) extends Ordinal {
  override def toString: String = i.toString
}

trait CNF_T extends Ordered[CNF_T] {

  import CNF_T._

  override def toString: String = this match {
    case CNF(_) => "w" +
      (if (firstExp > 1) "^"+firstExp.toString else "") +
      (if (firstCoeff > 1) "*"+firstCoeff.toString else "") +
      (if (rest > 0) "+"+rest.toString else "")
    case Atom(i) => i.toString()
  }

  override def compare(that: CNF_T): Int = (this, that) match {
    case (Atom(i), Atom(j)) => i compare j
    case (_, Atom(_)) => 1
    case (Atom(_), _) => -1
    case _ if firstExp != that.firstExp => firstExp compare that.firstExp
    case _ if firstCoeff != that.firstCoeff => firstCoeff compare that.firstCoeff
    case _ => rest compare that.rest
  }

  def ===(that: CNF_T) = this == that

  def +(that: CNF_T): CNF_T = (this, that) match {
    case (Atom(i), Atom(j)) => i + j
    case _ if firstExp < that.firstExp => that
    case _ if firstExp == that.firstExp => (firstExp, firstCoeff + that.firstCoeff) ::: that.rest
    case _ => (firstExp, firstCoeff) ::: (rest + that)
  }

  def -(that: CNF_T): CNF_T = (this, that) match {
    case (Atom(i), Atom(j)) => if (i < j) 0 else i - j
    case _ if firstExp < that.firstExp => 0
    case _ if firstExp > that.firstExp => this
    case _ if firstCoeff < that.firstCoeff => 0
    case _ if firstCoeff > that.firstCoeff => (firstExp, firstCoeff - that.firstCoeff) ::: rest
    case _ => rest - that.rest
  }

  def *(that: CNF_T): CNF_T = (this, that) match {
    case (a, b) if a == zero || b == zero => 0
    case (Atom(i), Atom(j)) => i * j
    case (_, Atom(j)) => (firstExp, firstCoeff * that) ::: rest
    case _ => (firstExp + that.firstExp, that.firstCoeff) ::: (this * that.rest)
  }

  def ^(that: CNF_T): CNF_T = (this, that) match {
    case _ if this == one || that == zero => 1
    case _ if this == zero => 0
    case (Atom(i), Atom(j)) => i.exp(j)
    case (a@Atom(_), _) => exp1(a, that)
    case (_, a@Atom(_)) => exp3(this, a)
    case _ => exp4(this, that)
  }

  def first: CNF_T = this match {
    case Atom(_) => 0
    case CNF((x, _)) => x
  }

  def rest: CNF_T = this match {
    case Atom(_) => 0
    case CNF((_, x)) => x
  }

  def firstExp: CNF_T = this match {
    case Atom(_) => 0
    case _ => first.first
  }

  def firstCoeff: CNF_T = this match {
    case Atom(i) => this
    case _ => first.rest
  }

  def isLimit: Boolean = this match {
    case Atom(i) => i == BigInt(0)
    case _ => rest.isLimit
  }

  def natPart: Atom = this match {
    case a@Atom(_) => a
    case _ => rest.natPart
  }


  def limitpart: CNF_T = this match {
    case Atom(_) => 0
    case _ => first :: rest.limitpart
  }

  def :::(p: (CNF_T, CNF_T)): CNF = CNF(p._1, p._2) :: this

  def ::(c: CNF_T): CNF = CNF(c, this)
}

case class CNF(cnf: (CNF_T, CNF_T)) extends CNF_T

case class Atom(i: BigInt) extends CNF_T

object CNF_T {
  def exp1(a: Atom, pow: CNF_T): CNF_T = pow match {
    case _ if pow.firstExp == one => val atom = pow.firstExp match {
      case a@Atom(_) => a
    }
      (pow.firstCoeff, Atom(a.i.exp(atom.i))) ::: 0
    case _ if (pow.rest match {
      case Atom(_) => true
      case _ => false
    }) => val atom = pow.rest match {
      case a@Atom(_) => a
    }
      ((pow.firstExp - 1, pow.firstCoeff) ::: 0) :: Atom(a.i.exp(atom.i)) :: 0
    case _ => val c = exp1(a, pow.rest)
      (pow.firstExp - 1, Atom(1)) ::: c.firstExp :: c.firstCoeff :: 0
  }

  def exp2(a: CNF_T, pow: Atom): CNF_T = pow match {
    case _ if pow == one => a
    case _ => ((a.firstExp * (pow - 1), Atom(1)) ::: 0) * a
  }

  def exp3(a: CNF_T, pow: Atom): CNF_T = pow match {
    case _ if pow == zero => 1
    case _ if pow == one => a
    case _ if a.isLimit => exp2(a, pow)
    case Atom(n) => exp3(a, n - 1) * a
  }

  /** Infinite ordinal to possibly infinite power  */
  def exp4(a: CNF_T, pow: CNF_T): CNF_T = ((a.firstExp * pow.limitpart, Atom(1)) ::: 0) * exp3(a, pow.natPart)
}