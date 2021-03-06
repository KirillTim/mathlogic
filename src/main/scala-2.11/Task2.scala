import java.io.{File, PrintWriter}

import propositional.{Checker, Deductor, Expr}
import propositional.Types.Statement

object Task2 {
  def main(args: Array[String]): Unit = {
    //val fileName = if (args.length == 0 || args(0) == "") "data/HW2/contra2.in" else args(0)
    val fileName = "data/HW2/test.in"
    val start = System.currentTimeMillis()
    val pw = new PrintWriter(new File(fileName + ".hw2.out"))
    new Deductor().apply(fileName) match {
      case Left(error) =>
        pw.write(error.toString)
      case Right(correct) =>
        pw.write(correct._1.mkString(",")+"|-"+correct._2+"\n")
        correct._3.foreach((line: Expr) => {
          pw.write(line+ "\n")
        })
    }
    val total = System.currentTimeMillis() - start
    println("time: " + (total / 1000.0) + "s")
    pw.close()
  }
}