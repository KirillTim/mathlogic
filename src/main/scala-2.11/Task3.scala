//import propositional.{ProofMaker, Types, Expr}
//import propositional.ExprTypes._
//import propositional.Types._
//
//import scala.collection.{ mutable, immutable}
//
//
//
//object Task3 {
//  def main(args: Array[String]): Unit = {
//
//    /*val q = (new Var("A") V new !!(new Var("A"))) & new Var("B") V new Var("C")// V new !!(new Var("C"))
//    val e = new ProofMaker()
//    val r = e.whenFalse(q)*/
//
//    /*def impl(a: Expr, b: Expr) = List(b ->: (a ->: b), b, a->: b)
//
//    def andFF(a: Expr, b:Expr) = List(((a&b)->:a)->:(((a&b)->: !!(a))->: !!(a & b)))
//    def t(a: Expr, b:Expr) = List(a & !!(b))
//    println(t(new Var("A"), new Var("B")).head)*/
//    val expr = new Var("A") & new Var("B") V new !!(new Var("B"))
//    val maker = new ProofMaker()
//    maker.apply(expr) match {
//      case (Left(a)) => println(a)
//      case (Right(proof)) => println("no error!")//proof.foreach((_:Expr) => println(_))
//    }
//  }
//}
