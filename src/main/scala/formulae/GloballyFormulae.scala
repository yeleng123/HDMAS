package formulae
import formulae.FormulaeType.{FormulaeType, GLOBALLY}
import formulae.OperatorType.OperatorType

class GloballyFormulae(nested:Formulae) extends PathFormulae(null,nested) {
  override def getFormulaType: FormulaeType = GLOBALLY

  override def getTempOperatorType: OperatorType = OperatorType.GLOBALLY


  override def stringTempOperator: String = "G "

  override def nnf(): Formulae = new GloballyFormulae(this.getRightFormula.nnf())

  override def negate(): Formulae = null//???
}
