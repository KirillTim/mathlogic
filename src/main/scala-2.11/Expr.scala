package Expr
trait Expr {
  def evaluate(m: Map[String, Boolean]): Boolean

  val priority: Int

  def str2(a: Expr, b: Expr, delim: String): String = {
    var aStr = a.toString
    var bStr = b.toString
    if (this.priority >= a.priority)
      aStr = "(" + aStr + ")"
    if (this.priority >= b.priority)
      bStr = "(" + bStr + ")"
    aStr + delim + bStr
  }
  override def equals(other: Any) : Boolean = {
    other.getClass == this.getClass && other.hashCode() == this.hashCode()
  }
  def ->: (other: Expr) : -> = new ->(other, this)
  def V(other: Expr) : V = new V(this, other)
  def &(other: Expr) : & = new &(this, other)
  def !!(other: Expr) : !! = new !!(this)
}

case class V(var a: Expr, var b: Expr) extends Expr {
  override def evaluate(m: Map[String, Boolean]): Boolean = a.evaluate(m) || b.evaluate(m)

  val priority: Int = 9

  override def toString: String = str2(a, b, "|")

  override def hashCode: Int = (a.hashCode * 12569) ^ (b.hashCode * 257)
}

case class &(var a: Expr, var b: Expr) extends Expr {
  override def evaluate(m: Map[String, Boolean]): Boolean = a.evaluate(m) && b.evaluate(m)

  val priority: Int = 10

  override def toString: String = str2(a, b, "&")

  override def hashCode: Int = (a.hashCode * 8647) ^ (b.hashCode * 257)
}

case class Const(x: Boolean) extends Expr {
  override def evaluate(m: Map[String, Boolean]): Boolean = x

  override val priority: Int = 20

  override def toString: String = x.toString

  override def hashCode(): Int = if (x) 1231 else 1237
}

case class Var(name: String) extends Expr {
  override def evaluate(m: Map[String, Boolean]): Boolean = {
    if (!(m contains name))
      throw new IllegalArgumentException("Can't find value for "+name)
    else
      (m get name).get
  }

  override val priority: Int = 20

  override def toString: String = name

  override def hashCode: Int = name.hashCode
}

case class ->(var a:Expr, var b:Expr) extends Expr {
  override def evaluate(m: Map[String, Boolean]): Boolean = (!a.evaluate(m)) || b.evaluate(m)

  override val priority: Int = 8

  override def toString: String = str2(a, b, "->")

  override def hashCode: Int = (a.hashCode * 15137) ^ (b.hashCode * 257)
}

case class !!(var a:Expr) extends Expr {
  override def evaluate(m: Map[String, Boolean]): Boolean = !a.evaluate(m)

  override val priority: Int = 11

  override def toString: String = a match {
    case v : Var => "!" + a.toString
    case v : Const => "!" + a.toString
    case _ => "!(" + a.toString + ")"
  }

  override def hashCode: Int = (a.hashCode ^ 29611) * 40241
}
