import java.io.{PrintWriter, File}

import Expr.{Expr, ->}

import scala.collection.mutable

object Task1 {
  def main(args: Array[String]) {
    val fileName = "data/task1.in"
    val lines = io.Source.fromFile(fileName).getLines().toSeq
    var result = mutable.MutableList[String]()
    var full = mutable.HashMap[Expr, Int]()
    var secondToFirst = mutable.HashMap[Expr, mutable.HashMap[Expr, Int]]()
    var lineNumber = 1
    for (line <- lines) {
      val tryExpr = new ExpressionParser(line).MainRule.run()
      var nextString = "(" + lineNumber + ") "
      if (tryExpr.isSuccess) {
        val expr = tryExpr.get
        expr match {
          case a -> b =>
            secondToFirst.get(b) match {
              case Some(list) => list += (a -> lineNumber)
              case None => secondToFirst += (b -> mutable.HashMap[Expr, Int](a -> lineNumber))
            }
          case _ =>
        }
        val num = Util.axiomNumber(expr)
        if (num != -1) {
          nextString += (line + " Сх. акс. " + num)
        } else {
          secondToFirst.get(expr) match {
            case Some(list) =>
              var res = Tuple2[Int, Int](-1, -1)
              for (i <- list) {
                if (full.contains(i._1)) {
                  res = (i._2, full.get(i._1).get)
                }
              }
              if (res._1 != -1) {
                nextString += (line + " M.P. " + res._2 + ", " + res._1)
              } else {
                nextString += (line + " Не доказуемо")
              }
            case None => nextString += (line + " Не доказуемо")
          }
        }
        full += (expr -> lineNumber)
      } else {
        nextString += ("Не могу прочитать выражение: " + line)
      }
      result += nextString
      lineNumber += 1
    }
    val pw = new PrintWriter(new File(fileName + ".hw1.out"))
    for (line <- result) {
      pw.write(line + "\n")
    }
    pw.close()
  }

}
