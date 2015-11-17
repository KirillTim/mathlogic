import propositional.ExprTypes._


var a = new Conj(new Var("A"), new Var("B"))
if (!a.isInstanceOf[Disj])
  println("not")