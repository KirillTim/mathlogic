import propositional.Expr
import propositional.ExprTypes.Var
import propositional.Types._

import scala.collection.{ mutable, immutable}



object Task3 {
  def main(args: Array[String]): Unit = {
    /*val a  = immutable.List[Int](1,2,3)
    val b = immutable.List(11,12,13)
    val c = immutable.List[immutable.List[Int]](a,b)
    println(a)
    println(b)
    println(c.flatten)*/
    val a = List(List(1,2), List(10,11), List(100,101))
    val b = a.flatMap((a:List[Int]) => a)
    val c = a.flatten
    println(b)
    println(c)

  }
}
