import propositional.{Checker, ExpressionParser, ProofMaker}

object Task3Test {
  def main(args: Array[String]) {
    val dataTrue = List(
      "A->A",
      "(A->!A)|(!A->A)",
      "(!A|!B)->!(B&A)",
      "!!(!!P->P)",
      "((A->B)&(B->A))|((A->B)&B|A|!B)&(B&!A|A)|A",
      "(A->B->C)->(B->A->C)",
      "(P->Q)|(!P->Q)",
      "A&B->A&B",
      "(A|!A)|B",
      "B&(B|!A)&C->C"
    )
    val dataFalse = List(
      "A->!A",
      "B",
      "B&(B|!A)&(C->A)"
    )
    testTrue(dataTrue)
    testFalse(dataFalse)
  }

  def testFalse(data:List[String]) = {
    println("testFalse")
    val proofMaker = new ProofMaker
    for (line <- data) {
      val parsed = new ExpressionParser(line.replaceAll(" ", "")).inputLine.run().get
      proofMaker.apply(parsed) match {
        case Left(error) => println(line + "\t[OK] : " + error)
        case Right(_) => println(line + "[Failed] proof for false expression generated")
      }
    }
  }

  def testTrue(data:List[String]) = {
    println("testTrue")
    val proofMaker = new ProofMaker
    for (line <- data) {
      val parsed = new ExpressionParser(line.replaceAll(" ", "")).inputLine.run().get
      try {
        proofMaker.apply(parsed) match {
          case Left(error) => println(line + "\t[Failed] : " + error)
          case Right(proof) =>
            new Checker().apply(proof) match {
              case Left(error) => println(line + "\t[Failed] : " + error)
              case Right(_) => println(line + "\t[OK]")
            }
        }
      }
      catch {
        case error:Error => println(line + "\t[Failed] : " + error)
      }
    }
  }
}
