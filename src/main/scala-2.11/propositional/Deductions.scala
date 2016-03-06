package propositional

import propositional.ExprTypes.Term

object Deductions {

  val A = new Term("A")
  val B = new Term("B")
  val C = new Term("C")

  private lazy val lemma1FAProof = lemma1InferFA(A,B,C)

  private def lemma1InferFA(a: Expr, b: Expr, c: Expr): List[Expr] = { //(A&B->FA(x,C)) -> (A->B->FA(x,C))
    new Deductor().apply(List((a&b) ->: c, a, b), Some(c), List(a, b, a ->: (b->:(a&b)), b ->: (a&b), a & b, (a&b) ->: c, c)) match {
      case Right(proof1) =>
        new Deductor().apply(List((a&b) ->: c, a), Some(b ->: c), proof1.map(st => st.expr)) match {
          case Right(proof2) =>
            new Deductor().apply(List((a&b) ->: c), Some(a ->: (b ->: c)), proof2.map(st => st.expr)) match {
              case Right(finalProof) => finalProof.map(st => st.expr).toList
              case Left(error) => throw new IllegalArgumentException("cant deduce third stage in lemma1 FA(" + a + "," + b + "," + c + ")  : " + error)
            }
          case Left(error) => throw new IllegalArgumentException("cant deduce second stage in lemma1 FA(" + a + "," + b + "," + c + ")  : " + error)
        }
      case Left(error) => throw new IllegalArgumentException("cant deduce in lemma1 FA(" + a + "," + b + "," + c + ")  : " + error)
    }
  }

  def lemma1FA(a: Expr, b: Expr, c: Expr) =
    lemma1FAProof.map(p => p.substitute(Map("A" -> a, "B" -> b, "C" -> c)))

  private lazy val lemma2FAProof = lemma2InferFA(A,B,C)

  private def lemma2InferFA(a: Expr, b: Expr, c: Expr): List[Expr] = { //(A->B->C) -> (A&B->C)
    new Deductor().apply(List(a ->: b ->: c, a&b), Some(c), List((a&b) ->: a, a&b, a, (a&b) ->: b, b, a ->: (b ->: c), b ->: c, c)) match {
        case Right(proof1) =>
          new Deductor().apply(List(a ->: b ->: c), Some(a&b ->: c), proof1.map(st => st.expr)) match {
            case Right(finalProof) => finalProof.map(st => st.expr).toList
            case Left(error) => throw new IllegalArgumentException("cant deduce second stage in lemma2 FA(" + a + "," + b + "," + c + ")  : " + error)
          }
        case Left(error) => throw new IllegalArgumentException("cant deduce in lemma2 FA(" + a + "," + b + "," + c + ")  : " + error)
      }
  }

  def lemma2FA(a: Expr, b: Expr, c: Expr) =
    lemma2FAProof.map(p => p.substitute(Map("A" -> a, "B" -> b, "C" -> c)))


  def lemmaEX(a: Expr, b: Expr, c: Expr): List[Expr] = { //(A->B->C) -> (B->A->C)
    new Deductor().apply(List(a ->: (b ->: c), b, a), Some(c), List(a ->: (b ->: c), a, b ->: c, b, c)) match {
      case Right(proof1) =>
        new Deductor().apply(List(a ->: (b ->: c), b), Some(a ->: c), proof1.map(st => st.expr)) match {
          case Right(proof2) =>
            new Deductor().apply(List(a ->: (b ->: c)), Some(b ->: (a ->: c)), proof2.map(st => st.expr)) match {
              case Right(finalProof) => finalProof.map(st => st.expr).toList
              case Left(error) => throw new IllegalArgumentException("cant deduce third stage in lemmaInferEX(" + a + "," + b + "," + c + ")  : " + error)
            }
          case Left(error) => throw new IllegalArgumentException("cant deduce second stage in lemmaInferEX(" + a + "," + b + "," + c + ")  : " + error)
        }
      case Left(error) => throw new IllegalArgumentException("cant deduce in lemmaInferEX(" + a + "," + b + "," + c + ")  : " + error)
    }
  }
}
