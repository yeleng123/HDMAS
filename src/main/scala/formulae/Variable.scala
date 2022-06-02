package formulae

import ap.parser._
import IExpression._
import ap.terfor.VariableTerm
import ap.types.Sort.Nat
import formulae.FormulaeType.{FormulaeType, VARIABLE}

class Variable(n:Int) extends Term(n) {

  override def getTermType: FormulaeType = {
    VARIABLE
  }

  //def getIndex = this.index

  override def toString: String = {
    "y" + super.getName.toString
  }

  override def toPrincess:IConstant = IConstant(new ConstantTerm(this.toString))

}


object Variable{
  def main(args: Array[String]): Unit ={
    //val y1:Variable = new Variable(1)
    //val y2:Variable = new Variable(2)
    //println(y1==y2)
  }
}
