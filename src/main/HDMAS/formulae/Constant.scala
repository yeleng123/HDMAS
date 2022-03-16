package formulae
import formulae.FormulaeType.{CONSTANT, FormulaeType}
//import scala.beans.BeanProperty

class Constant(name:Any) extends Term(name){

  //def Constant(name:String) = super(name)

  override def getTermType: FormulaeType = CONSTANT

  override def toString: String = super.getName.toString

}
