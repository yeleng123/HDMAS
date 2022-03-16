package formulae

import formulae.FormulaeType.{FormulaeType, VARIABLE}

class Variable(n:String) extends Term(n) {

  override def getTermType: FormulaeType = {
    VARIABLE
  }

  override def toString: String = {
    "?" + super.getName
  }
}


object Variable{
  def main(args: Array[String]): Unit ={
    val y1:Variable = new Variable("y1")
    val y2:Variable = new Variable("y1")
    println(y1==y2)
  }
}
