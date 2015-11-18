package propositional

import propositional.ExprTypes.{!!, Var}
import propositional.Types.{Statement, NotTrue, ErrorReason, Proof}
import scala.collection.{mutable => m}
import propositional.Proofs.buildProof

class ProofMaker {
  type Hypothesis = Map[String, Boolean]

  def apply(fileName: String): Either[ErrorReason, Proof] = {
    null
  }

  def apply(expr: Expr): Either[ErrorReason, List[Expr]] = {
    whenFalse(expr) match {
      case Some(q: Map[String, Boolean]) => Left(new NotTrue(varsToString(q)))
      case _ => Right(makeNotAnnotatedProof(expr).toList)
    }
  }

  def makeNotAnnotatedProof(expr: Expr):Seq[Expr] = {
    var curVars = expr.getVars
    var proofs = createVarsToBoolList(curVars).map( (hypot:Hypothesis) => hypot -> buildProof(expr, hypot)).toMap
    while (curVars.nonEmpty) {
      val tp = removeAssumption(curVars, proofs, expr)
      proofs = tp._2
      curVars = tp._1
    }
    proofs.head._2
  }

  def removeAssumption(vars: m.HashSet[String], proofs: Map[Hypothesis, Seq[Expr]],
                       beta: Expr): (m.HashSet[String], Map[Hypothesis, Seq[Expr]]) = {
    vars.isEmpty match {
      case true => (vars, proofs)
      case false => {
        val smallerVars = vars.take(vars.size - 1)
        val smallerVarsList = createVarsToBoolList(smallerVars)
        var newProofs = Map[Hypothesis, Seq[Expr]]()
        for (i <- smallerVarsList) {
          var proofsToMerge = m.MutableList[Seq[Expr]]()
          var varToRemove = ""
          for (j <- proofs) {
            val hypot = j._1
            if (checkVars(i, hypot)) {
              proofsToMerge += j._2
              varToRemove = hypot.keySet.filterNot(i.keySet).head
            }
          }
          //proofsToMerge.size == 2
          if (proofsToMerge.size != 2)
            throw new Error("proofsToMerge.size != 2")
          if (varToRemove.isEmpty)
            throw new Error("varToRemove.isEmpty")
          var newProof = List[Expr]()
          val deduceContext = i.keys.map((e: String) => new Var(e)).toSeq
          val deduced0 = new Deductor().apply(deduceContext, Some(proofsToMerge.head.last), proofsToMerge.head) match {
            case Left(proof) => {throw new Error("wrong proof"); null}
            case Right(proof) => (smallerVars, proof.map((a: Statement) => a.expr))
          }
          val deduced1 = new Deductor().apply(deduceContext, Some(proofsToMerge(1).last), proofsToMerge(1)) match {
            case Left(proof) => {throw new Error("wrong proof"); null}
            case Right(proof) => proof.map((a: Statement) => a.expr)
          }
          newProof ++= (deduced0 ++ deduced1 ++ Proofs.tertiumNonDatur(new Var(varToRemove)))
          val pVar = new Var(varToRemove)
          newProof ++= List[Expr](
            (pVar ->: beta) ->: (!!(pVar) ->: beta) ->: (pVar V !!(pVar)) ->: beta,
            (!!(pVar) ->: beta) ->: ((pVar V !!(pVar)) ->: beta),
            (pVar V !!(pVar)) ->: beta,
            beta)
          newProofs += (i -> newProof.toSeq)//.put(i, newProof.toSeq)
        }
        (smallerVars, newProofs)
      }
    }
  }
  //проверка, что все ключи из первого map есть во втором и все ключи из первого имеют одинаковые занчения в первом и втором
  //первый меньше чем второй
  def checkVars(first: Hypothesis, second: Hypothesis): Boolean = {
    for (i <- first) {
      second.get(i._1) match {
        case Some(a) => if (a != i._2) return false
        case _ => return false
      }
    }
    true
  }

  def createVarsToBoolList(vars: m.HashSet[String]): List[Hypothesis] = {
    gen(vars.size).map(x => vars.zip(x).toMap)
  }

  /**
    *
    * @param expr
    * @return Option[Map[String, Boolean] ]
    */
  def whenFalse(expr: Expr): Option[Hypothesis] = {
    //val vars = expr.getVars
    //gen(vars.size).map(x => vars.zip(x).toMap)
    createVarsToBoolList(expr.getVars)
      .map((x: Hypothesis) => if (!expr.evaluate(x)) Some(x) else None)
      .find({
        case Some(_) => true
        case _ => false
      }) match {
      case Some(a) => a //a.get
      case _ => None
    }
  }

  private def varsToString(m: Map[String, Boolean]): String = {
    var q = ""
    m.keys.foreach((x: String) => q += (x + "=" + (if (m.get(x).get) "И" else "Л") + ", "))
    q.take(q.length - 2)
  }

  private def gen(i: Int): List[List[Boolean]] = i match {
    case 1 => List(List(false), List(true))
    case _ => gen(i - 1).flatMap(x => List(x :+ false, x :+ true))
  }

}
