package object ordinals {
  def zero: Atom = 0

  def one: Atom = 1

  implicit def nat2atom(n: BigInt): Atom = Atom(n)

  implicit def int2atom(n: Int): Atom = Atom(n)

  implicit def seq2tuple(seq: CNF_T*): CNF = {
    if (seq.length != 2) throw new IllegalArgumentException()
    else CNF(seq(0) -> seq(1))
  }

  implicit class BigIntPow(b: BigInt) {
    def exp(n: BigInt): BigInt = {
      if (n.isValidInt) b.pow(n.intValue())
      else throw new IllegalArgumentException("Too big.")
    }
  }

  implicit def ord2cnf(ord: Ordinal): CNF_T = ord match {
    case W() => CNF(CNF(1, 1), 0)
    case Nat(n) => Atom(n)
    case +(a, b) => ord2cnf(a) + ord2cnf(b)
    case *(a, b) => ord2cnf(a) * ord2cnf(b)
    case ^(a, b) => ord2cnf(a) ^ ord2cnf(b)
  }
}