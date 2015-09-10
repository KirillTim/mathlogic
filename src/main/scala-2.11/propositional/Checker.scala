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
    for (i <- candidates) {
      for (p <- proof) {
        if (p.expr.equals(i._1) && p.annotation.getClass != Error.getClass) {
          //res = (p.line, i._2)
          return Some((p.line, i._2))
        }
      }
    }
    None
  }

  def apply(proofToCheck: Seq[Try[Expr]]): Proof = {
    var lineNumber = 1
    proofToCheck.foreach((tryExpr: Try[Expr]) => {
      if (tryExpr.isSuccess) {
        val expr = tryExpr.get
        expr match {
          case a -> b =>
            addMP(a, b, lineNumber)
          case _ =>
        }
        val num = Util.axiomNumber(expr)
        if (num != -1) {
          proof.append(new Statement(lineNumber, expr, new Axiom(num)))
        } else {
          secondToFirst.get(expr) match {
            case Some(list) =>
              val res = findMP(expr, proof, list)
              res match {
                case Some(data) =>
                  proof.append(new Statement(lineNumber, expr, new MP(data._1, data._2)))
                case _ =>
                  proof.append(new Statement(lineNumber, expr, new Error()))
              }
            case None => proof += new Statement(lineNumber, expr, new Error())
          }
        }
      }
      else {
        proof += new Statement(lineNumber, null, new Error("Не могу прочитать выражение:("))
      }
      lineNumber += 1
    })
    proof
  }

  def apply(fileName: String): Proof = {
    var exprs = new m.MutableList[Try[Expr]]
    io.Source.fromFile(fileName).getLines().foreach((line: String) =>
      exprs += new ExpressionParser(line.replaceAll(" ", "")).MainRule.run())
    apply(exprs)
    /*io.Source.fromFile(fileName).getLines().foreach((line: String) => {
      val tryExpr = new ExpressionParser(line).MainRule.run()
      if (tryExpr.isSuccess) {
        val expr = tryExpr.get
        expr match {
          case a -> b =>
            addMP(a, b, lineNumber)
          case _ =>
        }
        val num = Util.axiomNumber(expr)
        if (num != -1) {
          proof += new Statement(lineNumber, expr, new Axiom(num))
        } else {
          secondToFirst.get(expr) match {
            case Some(list) =>
              var res = findMP(expr, proof, list)
              res match {
                case Some(data) =>
                  proof += new Statement(lineNumber, expr, new MP(data._1, data._2))
                case _ =>
                  proof += new Statement(lineNumber, expr, new Error())
              }
              /*if (res._1 != -1) {
                proof += new Statement(lineNumber, expr, new MP(res._1, res._2))
              } else {
                proof += new Statement(lineNumber, expr, new Error())
              }*/
            case None => proof += new Statement(lineNumber, expr, new Error())
          }
        }
      }
      else {
        proof += new Statement(lineNumber, null, new Error("Не могу прочитать выражение: " + line))
      }
      lineNumber += 1
    })
    proof*/
  }
}

