object Task8 {
  def main(args: Array[String]) {
    val a = 1
    a match {
      case x if x >= 5 => println(">= 5")
      case x if x >= 10 => println(">= 10")
      case _ => ???
    }
  }
}
