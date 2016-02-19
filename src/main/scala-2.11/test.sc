def test(a:Int, b:Int*) = {
  println(b.isEmpty)
  println("a="+a+", b:"+b.mkString(","))
}

test(1)