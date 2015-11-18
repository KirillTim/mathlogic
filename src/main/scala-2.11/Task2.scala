import java.io.{File, PrintWriter}

import propositional.{Deductor, Checker}
import propositional.Types.Statement

object Task2 {
  def main(args: Array[String]): Unit = {
    val fileName = if (args.length == 0 || args(0) == "") "data/HW2/contra2.in" else args(0)
    val start = System.currentTimeMillis()
    val pw = new PrintWriter(new File(fileName + ".hw2.out"))
    (new Deductor().apply(fileName) match {
      case Left(error) =>
        pw.write("Доказательство неверно\n")
        error
      case Right(correct) =>
        correct
    }).foreach((line: Statement) => {
      pw.write(line + "\n")
    })
    val total = System.currentTimeMillis() - start
    println("time: " + (total / 1000.0) + "s")
    pw.close()
  }
}