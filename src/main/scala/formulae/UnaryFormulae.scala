package formulae
import formulae.FormulaeType.{FormulaeType, NOT}

//import scala.beans.BeanProperty


abstract class UnaryFormulae(nf:Formulae) extends Formulae {

  //@BeanProperty val nestedFormula:Formulae = nf
  val nestedFormula:Formulae = nf

  def getNestedFormula:Formulae = this.nestedFormula

  override def toString: String = {
    this.stringOperator + "(" +getNestedFormula + ")"
  }

  override def hashCode(): Int = {
    if (this.getNestedFormula != null) this.getNestedFormula.hashCode() else 0
  }

  override def equals(obj: Any): Boolean = {
    var res:Boolean = false

    if (obj != null && this.getClass.equals(obj.getClass)) {
      val other:UnaryFormulae = obj.asInstanceOf[UnaryFormulae]
      res = this.getNestedFormula.equals(other.getNestedFormula)
    }
    res
  }

  def formulaFactory(formulaType:FormulaeType,nested:Formulae):Formulae = formulaType match {
    case NOT => new NotFormulae(nested)
    case _ => throw new RuntimeException("Unknown formula type")
  }

  override def copy(): Formulae = {
    this.formulaFactory(this.getFormulaType,this.getNestedFormula.copy())
  }

  def stringOperator:String

}
