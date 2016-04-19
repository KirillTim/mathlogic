import java.io.{File, PrintWriter}

import propositional.{Deductor, Expr}
import propositional.Types._


object Task4 {
  val TEST_FOLDER = System.getProperty("user.dir") + "/data/hw4tests"

  def main(args: Array[String]): Unit = {
    val testFiles = getListOfFiles(TEST_FOLDER)
    for (name <- testFiles) {
      val fileName = if (args.length == 0 || args(0) == "") name else args(0)
      print(fileName + " testing... ")
      val start = System.currentTimeMillis()
      val pw = new PrintWriter(new File(name + ".hw4.out"))
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

  def getListOfFiles(dir: String): List[String] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList.map(_.toString)
    } else {
      List[String]()
    }
  }
}
