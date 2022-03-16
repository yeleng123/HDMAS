package formulae

import formulae.FormulaeType.{FormulaeType, GLOBALLY, NEXT, UNTIL}
import formulae.OperatorType.OperatorType

//import scala.beans.BeanProperty

abstract class PathFormulae(left:Formulae, right:Formulae) extends Formulae {
  //@BeanProperty val leftFormula:Formulae = left
  //@BeanProperty val rightFormula:Formulae = right
  var leftFormula:Formulae = left
  var rightFormula:Formulae = right

  def getLeftFormula:Formulae = this.leftFormula
  def getRightFormula:Formulae = this.rightFormula

  def stringTempOperator:String
  def getTempOperatorType:OperatorType

  override def toString: String = {
    if (this.getLeftFormula != null){
      this.getLeftFormula.toString + this.stringTempOperator + this.getRightFormula.toString
    } else {
      this.stringTempOperator + this.getRightFormula.toString
    }
  }

  override def hashCode(): Int = {
    var res:Int = this.getClass.hashCode()
    res = res * 31
    res = res + (if(this.getLeftFormula != null) this.getLeftFormula.hashCode() else 0)
    res = res * 31
    res = res + (if(this.getRightFormula != null) this.getRightFormula.hashCode() else 0)
    res
  }

  override def equals(obj: Any): Boolean = {
    var res:Boolean = false

    if (this.getClass.equals(obj.getClass)) {
      val other:PathFormulae = obj.asInstanceOf[PathFormulae]
      res = this.stringTempOperator.equals(other.stringTempOperator) && this.getLeftFormula.equals(other.getLeftFormula) && this.getRightFormula.equals(other.getRightFormula)
    }
    res
  }

  def formulaFactory(ftype:FormulaeType, left:Formulae, right:Formulae) :Formulae = ftype match {
    case NEXT => new NextFormulae(right)
    case GLOBALLY => new GloballyFormulae(right)
    case UNTIL => new UntilFormulae(left,right)
    case _ => throw new RuntimeException("Unknown formula type")
  }

  override def substitute(oldOne: Variable, newOne: Term): Unit = {
    if (this.getLeftFormula == null) {
      this.rightFormula.substitute(oldOne,newOne)
    } else {
      this.leftFormula.substitute(oldOne,newOne)
      this.rightFormula.substitute(oldOne,newOne)
    }
  }

  override def copy(): Formulae = formulaFactory(this.getFormulaType,this.getLeftFormula.copy(),this.getRightFormula.copy())







}
