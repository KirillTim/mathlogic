package propositional

import Types._
import propositional.ExprTypes._

import scala.collection.{mutable => m}
import scala.util.Try

class Checker {
  var lineNumber = 1
  val proof = new Proof()
  val context = Seq[Expr]()
  var secondToFirst = m.HashMap[Expr, m.HashMap[Expr, Int]]()

  type MultiMap[T, E] = m.HashMap[T, m.Set[E]] with m.MultiMap[T, E]
  val reversedImplications = new MultiMap[Expr, (Expr, Int)]

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

  def lineInProof(expr: Expr) : Option[Int] = {
    for (p <- proof if p.expr.equals(expr) && p.annotation.getClass != Error.getClass)
      return Some[p.line]
    None
  }

  def isProofed(expr:Expr) =
    lineInProof(expr).isDefined || context.contains(expr)

  def getMPAnnotation(expr: Expr): Either[WrongProof, Annotation] = expr match {
    case ->(phi, FA(x, psi)) if isProofed(phi ->: psi) =>
      phi.entersFree(x) match {
        case false => Right(new InferFA(lineInProof(expr).getOrElse(0))) //0 == expr was in assumption
        case true => Left(new InferenceRuleOnFreeVar(x, expr, lineNumber))
      }
    case ->(EX(x, psi), phi) if isProofed(psi ->: phi) =>
      psi.entersFree(x) match {
        case false => Right(new InferEX(lineInProof(expr).getOrElse(0)))
        case true => Left(new InferenceRuleOnFreeVar(x, expr, lineNumber))
      }
    case _ => Left(WrongProofFromLine(lineNumber, new Error().toString))
  }

  def apply2(context: Seq[Expr], beta: Option[Expr], proofToCheck: Seq[Expr]): Either[WrongProof, Proof] = {
    proofToCheck.foreach((expr: Expr) => {
      expr match {
        case a -> b => addImplication(a, b, lineNumber)
        case _ =>
      }
      val num = Util.axiomNumber(expr)
      if (num.isDefined) {
        proof += new Statement(lineNumber, expr, num.get)
      } else if (context.contains(expr)) {
        proof += new Statement(lineNumber, expr, new Assumption())
      } else {
        /*checkMPError(expr) match {
          case Some(reason) => return Left(reason)
          case _ =>
        }*/
        getMPAnnotation(expr) match {
          case Right(annotation) => proof += new Statement (lineNumber, expr, annotation)
          case Left(reason) => return Left(reason)
        }
      }
    })
    Right(proof)
  }

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

  def apply(context: Seq[Expr], beta: Option[Expr], proofToCheck: Seq[Expr]): Either[WrongProof, Proof] = {
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
      if (num.isDefined) {
        proof += new Statement(lineNumber, expr, num.get)
      } else if (context.contains(expr)) {
        proof += new Statement(lineNumber, expr, new Assumption())
      } else {
        secondToFirst.get(expr) match {
          case Some(list) =>
            val res = findMP(expr, proof, list)
            res match {
              case Some(data) =>
                proof += new Statement(lineNumber, expr, new MP(proof(data._1 - 1), proof(data._2 - 1)));
              case _ =>
                proof += new Statement(lineNumber, expr, new Error())
                return Left(WrongProofFromLine(lineNumber))
            }
          case None =>
            proof += new Statement(lineNumber, expr, new Error())
            return Left(WrongProofFromLine(lineNumber))
        }
      }
      lineNumber += 1
    })
    beta match {
      case Some(te) if te != proof.last.expr => return Left(WrongProofWithMsg("last statement doesn't equals to beta"))
      case _ =>
    }
    Right(proof)
  }

  def apply(proofToCheck: Seq[Expr]): Either[WrongProof, Proof] = {
    apply(List.empty, None, proofToCheck)
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

