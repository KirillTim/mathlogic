import ordinals._

import scala.io.Source

object Task8 {
  def main(args: Array[String]): Unit = {
    val parser = OrdinalsParser()
    for (s <- List("diff", "eq")) {
      val lines = Source.fromFile("data/HW8/"+s+".in").getLines()
      lines
        .map(parser.parse(_).get)
        .map { case (x, y) => println(s"$x = $y"); (x: CNF_T, y: CNF_T) }
        .foreach { case (x, y) => if (x == y) println(s"Равны. lhs = $x rhs = $y")
        else println(s"Не равны. lhs = $x, rhs = $y")
        }
    }
  }
}
