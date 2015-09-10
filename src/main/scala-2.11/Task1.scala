import java.io.{File, PrintWriter}

import propositional.Checker

object Task1 {
  def main(args: Array[String]): Unit = {
    val fileName = if (args.length == 0 || args(0) == "") "data/HW1/wrong6.in" else args(0)
    val start = System.currentTimeMillis()
    val result = new Checker().apply(fileName)
    val pw = new PrintWriter(new File(fileName + ".hw1.out"))
    for (line <- result) {
      pw.write(line + "\n")
    }
    val total = System.currentTimeMillis() - start
    println("time: "+(total/1000.0)+"s")
    pw.close()
  }

}
