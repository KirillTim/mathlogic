import propositional.ExprTypes.{EX, ->, Term, FA}
import propositional.{Deductor, Deductions, ExpressionParser, Checker}

object RandomTests {
  def main(args: Array[String]) {
    test1()
    test2()
    test3()
    test4()
    test5()
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
      case Left(err) => println("[Failed]")
      case Right(proof) => println("[OK]") //; print("p1:"+proof.length+"   ")
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
}
