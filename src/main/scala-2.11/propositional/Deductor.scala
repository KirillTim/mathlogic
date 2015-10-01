package propositional

import propositional.Types._

import scala.util.Try
import scala.collection.{mutable => m}

class Deductor {

  def deduce(proof: Proof, context: Seq[Expr], beta: Expr): Either[Proof, Proof] = {
    val alpha = context.last
    new Checker().apply(context.init, Some(context.last ->: beta), proof.map((st: Statement) => {
      st match {
        case Statement(_, e: Expr, a: Annotation) if e == alpha => case2(st, alpha)
        case Statement(_, e: Expr, a: Annotation) if e != alpha =>
          a match {
            case Axiom(_) => case1(st, alpha)
            case Assumption() => case1(st, alpha)
            case MP(j: Statement, k: Statement) => case3(st, j, alpha)
          }
        //case _ =>
      }
    }).flatMap((a:List[Expr]) => a))
  }

  private def case1(di: Statement, alpha: Expr): List[Expr] = {
    val line1 = di.expr ->: (alpha ->: di.expr)
    List(di.expr, line1, alpha ->: di.expr)
  }

  private def case2(di: Statement, alpha: Expr): List[Expr] = {
    val line1 = alpha ->: (alpha ->: alpha)
    val line2 = line1 ->: (alpha ->: ((alpha ->: alpha) ->: alpha)) ->: (alpha ->: alpha)
    val line3 = line2.b
    val line4 = alpha ->: ((alpha ->: alpha) ->: alpha)
    val line5 = alpha ->: alpha
    List(line1, line2, line3, line4, line5)
  }

  private def case3(di: Statement, dj:Statement, alpha: Expr): List[Expr] = {
    val line1 = (alpha ->: dj.expr) ->: ((alpha ->: (dj.expr ->: di.expr)) ->: (alpha ->: di.expr))
    val line2 = line1.b
    val line3 = alpha ->: di.expr
    List(line1, line2, line3)
  }

  def apply(fileName: String): Either[Proof, Proof] = {
    var proof = new m.MutableList[Expr]
    val lines = io.Source.fromFile(fileName).getLines().toList
    val firstLine = new ExpressionParser(lines.head.replaceAll(" ", "")).derivationInputLine.run()
    lines.tail.foreach((line: String) => {
      val parsed = new ExpressionParser(line.replaceAll(" ", "")).inputLine.run()
      if (parsed.isSuccess)
        proof += parsed.get
      else
        println("Не могу прочитать: " + line)
    })
    apply(firstLine.get._1, Some(firstLine.get._2), proof)
  }

  def apply(context: Seq[Expr], beta: Option[Expr], proof: Seq[Expr]): Either[Proof, Proof] = {
    if (beta.isEmpty) {
      println("Нужна бета!")
      return null
    }
    new Checker().apply(context, beta, proof) match {
      case Left(error) => Left(error)
      case Right(correct) => deduce(correct, context, beta.get)
    }
  }
}
