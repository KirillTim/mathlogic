package propositional

import Types._
import propositional.ExprTypes._

import scala.collection.{mutable => m}
import scala.util.Try

class Checker {
  var proof = new Proof()
  var secondToFirst = m.HashMap[Expr, m.HashMap[Expr, Int]]()

  def addMP(a: Expr, b: Expr, lineNumber: Int): Unit = {
    secondToFirst.get(b) match {
      case Some(list) => list += (a -> lineNumber)
      case None => secondToFirst += (b -> m.HashMap[Expr, Int](a -> lineNumber))
    }
  }

  def findMP(expr: Expr, proof: Proof, candidates: m.HashMap[Expr, Int]): Option[(Int, Int)] = {
    for (i <- candidates;
         p <- proof
         if p.expr.equals(i._1) && p.annotation.getClass != Error.getClass) {
      return Some((p.line, i._2))
    }
    None
  }

  def apply(context: Seq[Expr], beta: Option[Expr], proofToCheck: Seq[Expr]): Either[Proof, Proof] = {
    var proof = new Proof()
    var lineNumber = 1
    var error = false
    proofToCheck.foreach((expr: Expr) => {
        expr match {
          case a -> b =>
            addMP(a, b, lineNumber)
          case _ =>
        }
        val num = Util.axiomNumber(expr)
        if (num != -1) {
          proof += new Statement(lineNumber, expr, new Axiom(num))
        } else if (context.contains(expr)) {
          proof += new Statement(lineNumber, expr, new Assumption())
        } else {
          secondToFirst.get(expr) match {
            case Some(list) =>
              val res = findMP(expr, proof, list)
              res match {
                case Some(data) =>
                  //proof += new Statement(lineNumber, expr, new MP(data._1, data._2))
                  proof += new Statement(lineNumber, expr, new MP(proof(data._1-1), proof(data._2-1)));
                case _ =>
                  proof += new Statement(lineNumber, expr, new Error())
                  error = true
              }
            case None =>
              proof += new Statement(lineNumber, expr, new Error())
              error = true
          }
        }
      lineNumber += 1
    })
    beta match {
      case Some(te) if te != proof.last.expr => error = true
      case _ =>
    }
    if (error)
      Left(proof)
    else
      Right(proof)
  }

  def apply(proofToCheck: Seq[Expr]): Either[Proof, Proof] = {
    apply(List.empty, None, proofToCheck)
  }

  def apply(fileName: String): Either[Proof, Proof] = {
    var exprs = new m.MutableList[Expr]
    io.Source.fromFile(fileName).getLines().foreach((line: String) => {
      val parsed = new ExpressionParser(line.replaceAll(" ", "")).inputLine.run()
      if (parsed.isSuccess) {
        exprs += parsed.get
      } else {
        println("Не могу прочитать: " + line)
      }
    })
    apply(exprs)
  }
}

