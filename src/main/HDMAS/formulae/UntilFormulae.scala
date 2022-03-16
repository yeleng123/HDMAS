package formulae
import formulae.FormulaeType.FormulaeType
import formulae.OperatorType.{OperatorType, UNTIL}

class UntilFormulae(left:Formulae,right: Formulae) extends PathFormulae(left, right) {
  override def getTempOperatorType: OperatorType = UNTIL

  override def getFormulaType: FormulaeType = FormulaeType.UNTIL

  override def stringTempOperator: String = " U "

  override def nnf(): Formulae = new UntilFormulae(this.getLeftFormula.nnf(),this.getRightFormula.nnf())

  override def negate(): Formulae = null//？？？
}
