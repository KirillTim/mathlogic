import propositional.ExprTypes._
import scala.collection.{mutable => m}

def checkVars(first:Map[String, Boolean],second:Map[String,Boolean]) : Boolean = {
  for (i <- first) {
    second.get(i._1) match {
      case Some(a) => if (a != i._2) return false
      case _ => return false
    }
  }
  true
}

var a = Set(1,2,3)
var b = Set(1,2)
var c = a.filterNot(b)