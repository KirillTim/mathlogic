package propositional

import java.io.{File, PrintWriter}

import Types._
import propositional.ExprTypes._

import scala.collection.{mutable => m}

class Checker {

  var lineNumber = 1
  val proof = new Proof()
  var context: Seq[Expr] = null

  type MultiMap[T, E] = m.HashMap[T, m.Set[E]] with m.MultiMap[T, E]
  val reversedImplications = new m.HashMap[Expr, m.Set[(Expr, Int)]]() with m.MultiMap[Expr, (Expr, Int)] //new MultiMap[Expr, (Expr, Int)]()

  def addImplication(a: Expr, b: Expr, lineNumber: Int) = {
    reversedImplications.addBinding(b, (a, lineNumber))
  }

  def findMP(expr: Expr): Option[(Int, Int)] = {
    val candidates = reversedImplications.getOrElse(expr, Set())
    for (c <- candidates;
         p <- proof
         if p.expr.equals(c._1) && p.annotation.getClass != Error.getClass) {
      return Some((p.line, c._2))
    }
    None
  }

  def lineInProof(expr: Expr): Option[Int] = {
    for (p <- proof if p.expr.equals(expr) && p.annotation.getClass != Error.getClass)
      return Some(p.line)
    None
  }

  def isProofed(expr: Expr) =
    lineInProof(expr).isDefined || context.contains(expr)

  def getMPAnnotation(expr: Expr): Either[WrongProof, Annotation] = expr match {
    case ->(phi, FA(x, psi)) if isProofed(phi ->: psi) =>
      for (i <- context if i.varEntersFree(x))
        return Left(InferenceRuleOnFreeVar(x, i, lineNumber))
      phi.varEntersFree(x) match {
        case false => Right(new InferFA(lineInProof(expr).getOrElse(0))) //0 == expr was in assumption
        case true => Left(new EntersFreely(x, expr, lineNumber))
      }
    case ->(EX(x, psi), phi) if isProofed(psi ->: phi) =>
      for (i <- context if i.varEntersFree(x))
        return Left(InferenceRuleOnFreeVar(x, i, lineNumber))
      phi.varEntersFree(x) match {
        case false => Right(new InferEX(lineInProof(expr).getOrElse(0)))
        case true => Left(new EntersFreely(x, expr, lineNumber))
      }
    case _ => findMP(expr) match {
      case Some(pair) => Right(new MP(proof(pair._1 - 1), proof(pair._2 - 1)))
      case None => Left(WrongProofFromLine(lineNumber, "Выражение не может быть выведено"))
    }
  }

  def printLastProof(): Unit = {
    val fileName = "proof.tmp"
    val pw = new PrintWriter(new File(fileName))
    proof.foreach(s => pw.write(s+"\n"))
    pw.close()
  }


  def apply2(ctx: Seq[Expr], beta: Option[Expr], proofToCheck: Seq[Expr]): Either[WrongProof, Proof] = {
    context = ctx
    proofToCheck.foreach((expr: Expr) => {
      expr match {
        case a -> b => addImplication(a, b, lineNumber)
        case _ =>
      }
      Util.axiomNumber(expr, lineNumber) match {
        case Left(reason) => return Left(reason)
        case Right(num) =>
          if (num.isDefined) {
            proof += new Statement(lineNumber, expr, num.get)
          } else if (context.contains(expr)) {
            proof += new Statement(lineNumber, expr, new Assumption())
          } else {
            getMPAnnotation(expr) match {
              case Right(annotation) => proof += new Statement(lineNumber, expr, annotation)
              case Left(reason) => return Left(reason)
            }
          }
      }
      lineNumber += 1
    })
    Right(proof)
  }

  def apply(proofToCheck: Seq[Expr]): Either[WrongProof, Proof] = {
    apply2(List.empty, None, proofToCheck)
  }

  def apply(fileName: String): Either[WrongProof, Proof] = {
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

