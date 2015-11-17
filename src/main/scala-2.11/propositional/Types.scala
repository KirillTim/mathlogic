package propositional

import propositional.ExprTypes.Var

import scala.collection.{mutable => m}

object Types {
  case class Statement(line: Int, expr: Expr, annotation: Annotation) {

    override def toString = s"(" + line + ") " + expr + " " + annotation
  }

  type Proof = m.MutableList[Statement]

  trait Annotation {

  }

  case class Axiom(number: Int) extends Annotation {

    override def toString = s"Сх. акс. " + number
  }

  case class Assumption() extends Annotation {
    override def toString = "Предположение"
  }

  case class MP(first: Statement, second: Statement) extends Annotation {
    var ints = false
    var fi = 0
    var se = 0

    def this(f: Int, s: Int) = {
      this(null, null)
      ints = true
      fi = f
      se = s
    }

    //override def toString = s"M.P. " + first._1 + ", " + second._1
    override def toString = "M.P. " + (if (ints) fi + ", " + se else first.line + ", " + second.line)
  }

  case class Error(msg: String = "") extends Annotation {

    override def toString = s"Не доказано" + (if (msg != null && msg != "") ": " + msg else "")
  }

  trait ErrorReason {

  }

  case class NotTrue(values: String) extends ErrorReason {
    override def toString: String = "Высказывание ложно при " + values
  }

}
