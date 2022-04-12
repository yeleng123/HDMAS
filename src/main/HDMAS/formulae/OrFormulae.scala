package formulae

import ap.parser.IFormula
import formulae.BooleanConnectiveType.{BooleanConnectiveType, OR}
import formulae.FormulaeType.FormulaeType

class OrFormulae(left:Formulae,right:Formulae) extends BinaryFormulae(left,right) with BooleanOpFormulae {

  override def stringOperator(): String = {
    "OR"
  }

  override def getFormulaType: FormulaeType = FormulaeType.OR

  override def getOpType: BooleanConnectiveType = OR

  override def nnf(): formulae.Formulae = {
    val left:formulae.Formulae = this.getLeftFormula.nnf()
    val right:formulae.Formulae = this.getRightFormula.nnf()
    this.boolFormulaFactory(this.getOpType, left, right)
  }

  override def negate(): Formulae = {
    val left: formulae.Formulae = this.getLeftFormula.negate()
    val right: formulae.Formulae = this.getRightFormula.negate()
    this.boolFormulaFactory(BooleanConnectiveType.AND, left, right)
  }

  // override def toPrincess: IFormula = this.getLeftFormula.toPrincess | this.getRightFormula.toPrincess

}
