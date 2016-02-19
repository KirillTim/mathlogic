package propositional

import scala.collection.{mutable => m}

object Types {

  case class Statement(line: Int, expr: Expr, annotation: Annotation) {
    override lazy val toString = "(" + line + ") " + expr + " " + annotation
  }

  type Proof = m.MutableList[Statement]

  trait Annotation

  case class Axiom(number: Int) extends Annotation {
    override lazy val toString = "Сх. акс. " + number
  }

  case class Assumption() extends Annotation {
    override lazy val toString = "Предположение"
  }

  case class MP(first: Statement, second: Statement) extends Annotation {
    override lazy val toString = "M.P. " + first.line + ", " + second.line
  }

  case class Error(msg: String = "") extends Annotation {
    override lazy val toString = "Не доказано" + (if (msg != null && msg != "") ": " + msg else "")
  }

  trait ErrorReason

  trait WrongProof extends ErrorReason

  case class NotTrue(values: String) extends ErrorReason {
    override lazy val toString = "Высказывание ложно при " + values
  }

  case class WrongProofFromLine(lineNumber: Int) extends WrongProof {
    override lazy val toString = "Доказательство неверно со строки " + lineNumber
  }

  case class WrongProofWithMsg(msg: String) extends WrongProof {
    override lazy val toString = msg
  }

}
