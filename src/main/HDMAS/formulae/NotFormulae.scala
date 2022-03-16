package formulae
import formulae.BooleanConnectiveType.BooleanConnectiveType
import formulae.FormulaeType.{FormulaeType, NOT}

//import scala.beans.BeanProperty

class NotFormulae(nestedFormula:Formulae) extends UnaryFormulae(nestedFormula) with BooleanOpFormulae {


  override def getFormulaType: FormulaeType = NOT

  override def stringOperator: String = "NOT"

  override def getOpType: BooleanConnectiveType = BooleanConnectiveType.NOT

  override def nnf(): Formulae = {
    val nested:Formulae = this.getNestedFormula
    if (nested.isInstanceOf[AtomicFormulae]){
      this.copy()
    } else {
      nested.negate().nnf()
    }
  }

  override def negate(): Formulae = this.getNestedFormula.copy()

  override def substitute(oldOne: Variable, newOne: Term): Unit = this.nestedFormula.substitute(oldOne, newOne)
}
