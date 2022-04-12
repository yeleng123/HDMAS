package formulae

import ap.parser._
import IExpression._
import formulae.FormulaeType.{CONSTANT, FormulaeType}
//import scala.beans.BeanProperty

class Constant(name:Int) extends Term(name){

  //def Constant(name:String) = super(name)

  override def getTermType: FormulaeType = CONSTANT

  override def toString: String = super.getName.toString

  override def toPrincess:ITerm = Int2ITerm(this.getName)

}
