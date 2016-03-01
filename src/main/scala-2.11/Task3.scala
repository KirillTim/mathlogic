import java.io.{File, PrintWriter}

import propositional.{ExpressionParser, ProofMaker}



object Task3 {
  def main(args: Array[String]): Unit = {
    //val fileName = if (args.length == 0 || args(0) == "") "data/HW3/true1.in" else args(0)
    val fileName = "data/HW3/test.in"
    val lines = io.Source.fromFile(fileName).getLines().toList
    val parsed = new ExpressionParser(lines.head.replaceAll(" ", ""))
      .inputLine.run().get
    val maker = new ProofMaker()
    val pw = new PrintWriter(new File(fileName+".hw3.out"))

    maker.apply(parsed) match {
      case Left(reason) => pw.write(reason.toString)
      case Right(proof) => proof.foreach( e => pw.write(e+"\n"))
    }
    pw.close()
  }
}
