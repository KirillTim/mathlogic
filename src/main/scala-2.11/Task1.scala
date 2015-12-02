import java.io.{File, PrintWriter}

import propositional.Checker
import propositional.Types.{Statement, Proof}

object Task1 {
  def main(args: Array[String]): Unit = {
    val fileName = if (args.length == 0 || args(0) == "") "data/HW1/good4.in" else args(0)
    val start = System.currentTimeMillis()
    val pw = new PrintWriter(new File(fileName + ".hw1.out"))
    new Checker().apply(fileName) match {
      case Left(error) =>
        pw.write(error.toString)
      case Right(correct) =>
        correct.foreach((line: Statement) => {
          pw.write(line + "\n")
        })
    }
    val total = System.currentTimeMillis() - start
    println("time: " + (total / 1000.0) + "s")
    pw.close()
  }

}
