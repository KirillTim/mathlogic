package propositional

import propositional.ExprTypes.Term

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

  case class InferFA(lineNumber: Int) extends Annotation {
    override lazy val toString = "Правило для квантора всеобщности в строке " + lineNumber
  }

  case class InferEX(lineNumber: Int) extends Annotation {
    override lazy val toString = "Правило для квантора существования в строке " + lineNumber
  }


  case class Error(msg: String = "") extends Annotation {
    override lazy val toString = "Не доказано" + (if (msg != null && msg != "") ": " + msg else "")
  }

  trait ErrorReason

  trait WrongProof extends ErrorReason

  case class NotTrue(values: String) extends WrongProof {
    override lazy val toString = "Высказывание ложно при " + values
  }

  case class WrongProofFromLine(lineNumber: Int, msg:String = "") extends WrongProof {
    override lazy val toString = "Доказательство неверно со строки " + lineNumber + (if (msg != "") " ("+msg+")")
  }

  case class WrongProofWithMsg(msg: String) extends WrongProof {
    override lazy val toString = msg
  }

  case class NotFreeForSubstitution(t: Expr, x: Term, e: Expr, line: Int) extends WrongProof {
    override def toString = "В строке " + line + " терм " + t + " не свободен для подстановки вместо терма " + x + " формулу " + e
  }

  case class EntersFreely(x: Term, e: Expr, line: Int) extends WrongProof {
    override def toString = "В строке " + line + " переменная " + x + " входит свободно в формулу " + e
  }

  case class InferenceRuleOnFreeVar(t: Term, e: Expr, line: Int) extends WrongProof {
    override def toString = "В строке " + line + " используется правило вывода по переменной " + t + " входящей свободно в предположение " + e
  }

}
