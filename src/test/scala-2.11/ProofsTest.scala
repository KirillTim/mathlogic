import propositional.ExprTypes.{!!, Var}
import propositional.Proofs._
import propositional.{Expr, Checker}

object ProofsTest {
  val checker = new Checker()
  def main(args: Array[String]) {
    val varA = new Var("A")
    val varB = new Var("B")
    val contextFF = Seq(!!(varA), !!(varB))
    val contextFT = Seq(!!(varA), varB)
    val contextTF = Seq(varA, !!(varB))
    val contextTT = Seq(varA, varB)
    println("Or")
    isCorrect(contextFF, orFF(contextTT(0), contextTT(1)))
    isCorrect(contextFT, orFT(contextTT(0), contextTT(1)))
    isCorrect(contextTF, orTF(contextTT(0), contextTT(1)))
    isCorrect(contextTT, orTT(contextTT(0), contextTT(1)))

    println("And")
    isCorrect(contextFF, andFF(contextTT(0), contextTT(1)))
    isCorrect(contextFT, andFT(contextTT(0), contextTT(1)))
    isCorrect(contextTF, andTF(contextTT(0), contextTT(1)))
    isCorrect(contextTT, andTT(contextTT(0), contextTT(1)))

    println("Impl")
    isCorrect(contextFF, implFF(contextTT(0), contextTT(1)))
    isCorrect(contextFT, implFT(contextTT(0), contextTT(1)))
    isCorrect(contextTF, implTF(contextTT(0), contextTT(1), List("A"->true, "B"->false).toMap))
    isCorrect(contextTT, implTT(contextTT(0), contextTT(1)))

    println("Not")
    isCorrect(Seq(!!(varA)), notF(varA))
    isCorrect(Seq(varA), notT(varA))
  }

  def isCorrect(context:Seq[Expr], proof:List[Expr]) = {
    val res = checker.apply(context, None, proof)
    res match {
      case Left(error) => println(context + "\t[Failed] : " + error); false
      case Right(pr) => println(context + "\t[OK]"); true
    }
  }
}
