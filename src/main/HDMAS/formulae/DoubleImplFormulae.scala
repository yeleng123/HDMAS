package formulae
import formulae.BooleanConnectiveType.BooleanConnectiveType
import formulae.FormulaeType.{DOUBLE_IMPL, FormulaeType}

class DoubleImplFormulae(left:Formulae,right:Formulae) extends BinaryFormulae(left, right) with BooleanOpFormulae {

  override def stringOperator(): String = {
    "DIMPL"
  }

  override def getFormulaType: FormulaeType = DOUBLE_IMPL

  override def getOpType: BooleanConnectiveType = {
    BooleanConnectiveType.DOUBLE_IMP
  }

  override def nnf(): Formulae = {
    val left:Formulae = this.getLeftFormula.nnf()
    val right:Formulae = this.getRightFormula.nnf()
    val impl1:Formulae = this.boolFormulaFactory(BooleanConnectiveType.IMPL, left, right)
    val impl2:Formulae = this.boolFormulaFactory(BooleanConnectiveType.IMPL, right, left)
    this.boolFormulaFactory(BooleanConnectiveType.AND, impl1, impl2)
  }

  override def negate(): Formulae = {
    this.nnf().negate()
  }

}
