import propositional.ExprTypes._
import propositional.{Deductor, Deductions, ExpressionParser, Checker}
import propositional.Util.axiomNumber

object RandomTests {
  def main(args: Array[String]) {
    test1()
    test2()
    test3()
    test4()
    test5()
    test6()
    test7()
    test8()
    test9()
  }

  def test1(): Unit = {
    print("hw4/correct2.in test: ")
    val ctx = List(new ExpressionParser("P(b)").inputLine.run().get)
    val proof = List("P(b)",
      "P(b)->(A->A->A)->P(b)",
      "(A->A->A)->P(b)",
      "((A->A->A)->P(b))->P(a)->((A->A->A)->P(b))",
      "P(a)->(A->A->A)->P(b)",
      "?aP(a)->(A->A->A)->P(b)").map(s => new ExpressionParser(s).inputLine.run().get)
    new Checker().apply2(ctx, None, proof) match {
      case Left(err) => println("[Failed] " + err)
      case Right(_) => println("[OK]");
    }
  }

  def test2() = {
    print("deduction lemmas FA test: ")
    val a = new Term("A")
    val b = new Term("B")
    val c = new Term("P")
    new Checker().apply2(List(), None, Deductions.lemma1FA(a, b, FA(new Term("x"), c))) match {
      case Left(err) => print("[Failed] ")
      case Right(proof) => print("[OK] ") //; print("p1:"+proof.length+"   ")
    }
    new Checker().apply2(List(), None, Deductions.lemma2FA(a, b, c)) match {
      case Left(err) => println("[Failed]")
      case Right(proof) => println("[OK]") //; print("p2:"+proof.length+"    ")
    }
  }

  def test3() = {
    print("deduceFA test: ")
    val alpha = new Term("a")
    val P = new Term("P")
    val F = new Term("F")
    val assumption = alpha ->: (F ->: P)
    val what = F ->: FA(new Term("x"), P)
    val proof = new Deductor().deduceFA(what, alpha)
    new Checker().apply2(List(assumption, what), Some(alpha ->: what), proof) match {
      case Left(error) => println("[Failed]")
      case Right(_) => println("[OK]")
    }
  }

  def test4() = {
    print("deduction lemmas EX test: ")
    val a = new Term("A")
    val b = new Term("B")
    val c = new Term("P")
    new Checker().apply2(List(), None, Deductions.lemmaEX(a, b, c)) match {
      case Left(error) => println("[Failed]")
      case Right(proof) => println("[OK]")
    }
  }

  def test5() = {
    print("deduceEX test: ")
    val alpha = new Term("a")
    val P = new Term("P")
    val F = new Term("F")
    val assumption = alpha ->: (P ->: F)
    val what = EX(new Term("x"), P) ->: F
    val proof = new Deductor().deduceEX(what, alpha)
    new Checker().apply2(List(assumption, what), Some(alpha ->: what), proof) match {
      case Left(error) => println("[Failed]")
      case Right(_) => println("[OK]")
    }
  }

  def test6() = {
    print("brackets test: ")
    val e = new EX(new Term("x"), new Term("P", new Term("a")) ->: new Term("Q", new Term("x")) )
    if (e.toString.equals("?x(P(a)->Q(x))"))
      println("[OK]")
    else
      println("[Failed]")
  }

  def test7() = {
    print("substitution test: ")
    val a = new Term("a")
    val x = new Term("x")
    //substitute x for a
    val free = List("@xP(x)",
      "?x(x=x)",
      "?xP(x)->x",
      "?a(P(w)->P(a))",
      "x->x",
      "@xP(x)",
      "?x((x*0''')=(0'''*x))",
      "Q(a,b,x)->@x@a(Q(x)->a)").map(new ExpressionParser(_).inputLine.run().get)
    if (free.forall(p => p.isFreeForSubstitution(x, a)))
      print("[OK] ")
    else
      print("[Failed] ")
    val notFree = List("?a(P(x)->P(a))",
      "?x(Q(x))->@a(P(a)->Q(x))",
      "@aP(a,b,c,x)").map(new ExpressionParser(_).inputLine.run().get)
    if (notFree.forall(p => !p.isFreeForSubstitution(x, a)))
      println("[OK]")
    else
      println("[Failed] ")
  }

  def test8() = {
    print("enters free test: ")
    val x = new Term("x")
    val yes = List("x",
      "P(a,b,c,d)->Q(x,x)",
      "?xP(x)->x",
      "@a(P(a)->x)").map(new ExpressionParser(_).inputLine.run().get)
    if (yes.forall(p => p.entersFree(x)))
      print("[OK] ")
    else
      print("[Failed] ")
    val no = List("?x(x=0'')",
      "?dP(a,b,c,d)->?xQ(x,x)",
      "?x(P(x)->x)",
      "@a@x(P(a)->x)").map(new ExpressionParser(_).inputLine.run().get)
    if (no.forall(p => !p.entersFree(x)))
      println("[OK] ")
    else
      println("[Failed] ")
  }

  def test9() = {
    print("is substituted test: ")
    val a = new Term("a")
    val x = new Term("x")
    val yes = List("x" -> "q",
      "(x=x)" -> "(z=z)",
      "P(x)" -> "P(a)",
      "Q(x,y,z)" -> "Q(a,y,z)",
      "P(x,x)->?xQ(x)" -> "P(a,a)->?xQ(x)",
      "P(x,a)" -> "P(a,a)",
      "x&x"->"x&x"
      ).map(p => (new ExpressionParser(p._1).inputLine.run().get, new ExpressionParser(p._2).inputLine.run().get))
    if (yes.forall(p => p._1.isSubstituted(x, p._2)))
      print("[OK] ")
    else
      print("[Failed] ")
    val no = List("x" -> "q->q",
      "(x=x)" -> "(z=a)",
      "@z(P(x,y)->P(z))" -> "@z(P(x,z)->P(z))",
      "Q(x,y,z)" -> "Q(y,x,z)",
      "P(x,x)->?xQ(x)" -> "P(a,a)->?yQ(y)",
      "P(x,a)" -> "Q(a,a)",
      "x&a"->"x&x",
      "P(x)" -> "Q(a)"
    ).map(p => (new ExpressionParser(p._1).inputLine.run().get, new ExpressionParser(p._2).inputLine.run().get))
    if (no.forall(p => !p._1.isSubstituted(x, p._2)))
      println("[OK] ")
    else
      println("[Failed] ")
  }
}
