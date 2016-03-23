import java.io.{File, PrintWriter}

import propositional.{Checker, Deductor, Expr, ExpressionParser}
import propositional.Types._


object Task4 {
  def main(args: Array[String]): Unit = {
    //for (st <- Seq(/*"correct",*/ "incorrect");test <- (1 to 10).toList ++ (11 to 15).toList) {
    for (st <- Seq("test"); test <- 1 to 19) {
      val fileName = if (args.length == 0 || args(0) == "") "data/HW4/tests6/" + st + "."+test else args(0)
      print(fileName + " testing... ")
      val start = System.currentTimeMillis()
      val pw = new PrintWriter(new File(fileName + ".hw4.out"))
      try {
        new Deductor().apply(fileName) match {
          case Left(error) =>
            pw.write(
              (error match {
                case a@NotFreeForSubstitution(_, _, _, line) => WrongProofFromLine(line, a.toString)
                case a@EntersFreely(_, _, line) => WrongProofFromLine(line, a.toString)
                case a@InferenceRuleOnFreeVar(_, _, line) => WrongProofFromLine(line, a.toString)
                case _ => error
              }).toString)
            println("[Failed]")
          case Right(correct) =>
            pw.write(correct._1.mkString(",") + "|-" + correct._2 + "\n")
            correct._3.foreach((line: Expr) => pw.write(line + "\n"))
            println("[OK]")
        }
      } catch {
        case e: Throwable => println(e.getMessage)
      }
      val total = System.currentTimeMillis() - start
      println("time: " + (total / 1000.0) + "s")
      pw.close()

    }
  }
}