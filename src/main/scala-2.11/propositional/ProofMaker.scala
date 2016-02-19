package propositional

import propositional.ExprTypes.{!!, Not, Term}
import propositional.Proofs.buildProof
import propositional.Types.{ErrorReason, NotTrue, Proof, Statement}

import scala.collection.{mutable => m}

class ProofMaker {
  type Hypothesis = Map[String, Boolean]

  def apply(fileName: String): Either[ErrorReason, Proof] = {
    null
  }

  def apply(expr: Expr): Either[ErrorReason, List[Expr]] = {
    whenFalse(expr) match {
      case Some(q: Map[String, Boolean]) => Left(new NotTrue(varsToString(q)))
      case _ => Right(makeNotAnnotatedProof(expr))
    }
  }

  def makeNotAnnotatedProof(expr: Expr): List[Expr] = {
    var curVars = expr.getVars
    var proofs = createVarsToBoolList(curVars)
      .map((hypot: Hypothesis) => hypot -> buildProof(expr, hypot)).toMap
    while (curVars.nonEmpty) {
      val tp = removeAssumption(curVars, proofs, expr)
      curVars = tp._1
      proofs = tp._2
    }
    proofs.head._2
  }

  def removeAssumption(vars: m.HashSet[String], proofs: Map[Hypothesis, List[Expr]],
                       beta: Expr): (m.HashSet[String], Map[Hypothesis, List[Expr]]) = {
    //print("vars: ")
    //vars.foreach(s => print(s+", "))
    //println("beta= "+beta)
    val smallerVars = vars.init
    val smallerHypothesisList = createVarsToBoolList(smallerVars)
    var newProofs = Map[Hypothesis, List[Expr]]()
    smallerVars.isEmpty match {
      case true =>
        var deduceContext0 = List[Expr]()
        var deduceContext1 = List[Expr]()
        if (proofs.head._1.head._2) {
          deduceContext0 ++= List(new Term(proofs.head._1.head._1))
        } else {
          deduceContext0 ++= List(new Not(new Term(proofs.head._1.head._1)))
        }
        if (proofs.last._1.head._2) {
          deduceContext1 ++= List(new Term(proofs.last._1.head._1))
        } else {
          deduceContext1 ++= List(new Not(new Term(proofs.last._1.head._1)))
        }
        (smallerVars, List(Map.empty[String, Boolean]
          -> mergeProofs(deduceContext0, deduceContext1, new Term(vars.last), beta, proofs.head._2, proofs.last._2)).toMap)
      case false =>
        for (i <- smallerHypothesisList) {
          var proofsToMerge = m.MutableList[List[Expr]]()
          var deduceContext = m.MutableList[List[Expr]]()
          var varToRemove = ""
          for (j <- proofs) {
            val hypot = j._1
            if (checkVars(i, hypot)) {
              proofsToMerge += j._2
              varToRemove = hypot.keySet.filterNot(i.keySet).head
              var context = List[Expr]()
              for (h <- hypot if !h._1.equals(varToRemove)) {
                val varAddToContext = new Term(h._1)
                if (!h._2) {
                  context ++= List(new Not(varAddToContext))
                } else {
                  context ++= List(varAddToContext)
                }
              }
              context ++= List(if (hypot.get(varToRemove).get) new Term(varToRemove) else new Not(new Term(varToRemove)))
              deduceContext += context
            }
          }
          if (proofsToMerge.size != 2)
            throw new Error("proofsToMerge.size != 2")
          if (varToRemove.isEmpty)
            throw new Error("varToRemove.isEmpty")
          newProofs += (i -> mergeProofs(deduceContext.head.toSeq, deduceContext(1).toSeq,
            new Term(varToRemove), beta, proofsToMerge.head, proofsToMerge(1)))
        }
        (smallerVars, newProofs)
    }
  }


  def mergeProofs(firstDeduceContext: Seq[Expr], secondDeduceContext: Seq[Expr], varToRemove: Term, beta: Expr,
                  firstProof: List[Expr], secondProof: List[Expr]): List[Expr] = {
    var newProof = List[Expr]()
    val deduced0 = new Deductor().apply(firstDeduceContext, Some(firstProof.last), firstProof) match {
      case Left(error) => throw new Error("wrong proof in deduce0 : " + error);
      case Right(proof) => proof.map((a: Statement) => a.expr)
    }
    val deduced1 = new Deductor().apply(secondDeduceContext, Some(secondProof.last), secondProof) match {
      case Left(error) => throw new Error("wrong proof in deduce1 : " + error);
      case Right(proof) => proof.map((a: Statement) => a.expr)
    }
    newProof ++= (deduced0 ++ deduced1 ++ Proofs.tertiumNonDatur(varToRemove))
    newProof ++= List[Expr](
      (varToRemove ->: beta) ->: (!!(varToRemove) ->: beta) ->: (varToRemove V !!(varToRemove)) ->: beta,
      (!!(varToRemove) ->: beta) ->: ((varToRemove V !!(varToRemove)) ->: beta),
      (varToRemove V !!(varToRemove)) ->: beta,
      beta)
    newProof
  }


  /*проверка, что все ключи из первого map есть во втором и все ключи из первого имеют одинаковые занчения в первом и втором
   первый меньше чем второй */
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

  def whenFalse(expr: Expr): Option[Hypothesis] = {
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

  private def varsToString(m: Map[String, Boolean]) =
    m.keys.fold("") {
      (z, x) => z + x + "=" + (if (m.get(x).get) "И" else "Л") + ", "
    }.dropRight(2)

  private def gen(i: Int): List[List[Boolean]] = i match {
    case 0 => List()
    case 1 => List(List(false), List(true))
    case _ => gen(i - 1).flatMap(x => List(x :+ false, x :+ true))
  }

}
