package propositional

import propositional.ExprTypes.{Term, FA, ->, EX}
import propositional.Types._

import scala.util.Try
import scala.collection.{mutable => m}

class Deductor {

  def deduce(proof: Proof, context: Seq[Expr], beta: Expr): Either[WrongProof, Proof] = {
    val alpha = context.last

    var tmp = List[Expr]()
    println("proof.size = "+proof.size)
    proof.foreach((st: Statement) => {
      //println("len= "+tmp.length)
      val q = st match {
        case Statement(_, e: Expr, a: Annotation) if e == alpha => case2(st, alpha)
        case Statement(_, e: Expr, a: Annotation) if e != alpha =>
          a match {
            case Axiom(_) => case1(st, alpha)
            case Assumption() => case1(st, alpha)
            case MP(j: Statement, k: Statement) => case3(st, j, alpha)
            case InferFA(_) => caseFA(st, alpha)
            case InferEX(_) => caseEX(st, alpha)
          }
        //case _ =>
      }
      tmp = tmp ++ q
      if (tmp.size % 3000 == 0) {
        println("add "+tmp.size+" lines")
      }
    })

    new Checker().apply2(context.init, Some(context.last ->: beta), tmp)/*proof.map((st: Statement) => {
      //println("now at: "+st)
      st match {
        case Statement(_, e: Expr, a: Annotation) if e == alpha => case2(st, alpha)
        case Statement(_, e: Expr, a: Annotation) if e != alpha =>
          a match {
            case Axiom(_) => case1(st, alpha)
            case Assumption() => case1(st, alpha)
            case MP(j: Statement, k: Statement) => case3(st, j, alpha)
            case InferFA(_) => caseFA(st, alpha)
            case InferEX(_) => caseEX(st, alpha)
          }
        //case _ =>
      }
    }).flatMap((a: List[Expr]) => a))*/
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

  private def case3(di: Statement, dj: Statement, alpha: Expr): List[Expr] = {
    val line1 = (alpha ->: dj.expr) ->: ((alpha ->: (dj.expr ->: di.expr)) ->: (alpha ->: di.expr))
    val line2 = line1.b
    val line3 = alpha ->: di.expr
    List(line1, line2, line3)
  }

  def deduceFA(what:Expr, alpha: Expr): List[Expr] = {
    what match {
      case ->(phi, FA(x, psi)) =>
        Deductions.lemma1FA(alpha, phi, FA(x, psi)) ++
        Deductions.lemma2FA(alpha, phi, psi) ++
        List(
          alpha ->: (phi ->: psi),
          (alpha & phi) ->: psi,
          (alpha & phi) ->: FA(x, psi),
          alpha ->: (phi ->: FA(x, psi))
        )
      case _ => throw new IllegalArgumentException("wtf")
    }
  }

  private def caseFA(st: Statement, alpha: Expr): List[Expr] = {
    deduceFA(st.expr, alpha)
  }

  def deduceEX(what: Expr, alpha: Expr): List[Expr] = {
    what match {
      case ->(EX(x, psi), phi) =>
        Deductions.lemmaEX(alpha, psi, phi) ++
          List(alpha ->: (psi ->: phi),
            psi ->: (alpha ->: phi),
            EX(x, psi) ->: (alpha ->: phi)) ++
          Deductions.lemmaEX(EX(x, psi) , alpha, phi) ++
          List(alpha ->: (EX(x, psi) ->: phi))
      case _ => throw new IllegalArgumentException("wtf")
    }
  }

  private def caseEX(st: Statement, alpha: Expr): List[Expr] = {
    deduceEX(st.expr, alpha)
  }

  def apply(fileName: String): Either[WrongProof, Proof] = {
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

  def apply(context: Seq[Expr], beta: Option[Expr], proof: Seq[Expr]): Either[WrongProof, Proof] = {
    if (beta.isEmpty) {
      println("Нужна бета!")
      return null
    }
    new Checker().apply2(context, beta, proof) match {
      case Left(error) => Left(error)
      case Right(correct) => deduce(correct, context, beta.get)
    }
  }
}
