package formulae
import ap.parser.IFormula
import formulae.BooleanConnectiveType.BooleanConnectiveType
import formulae.FormulaeType.{FormulaeType, IMPLICATION}

class ImpFormulae(left:Formulae, right:Formulae) extends BinaryFormulae(left,right) with BooleanOpFormulae {

  override def stringOperator(): String = {
    "IMPL"
  }

  override def getFormulaType: FormulaeType = IMPLICATION

  override def getOpType: BooleanConnectiveType = {
    BooleanConnectiveType.IMPL
  }

  override def nnf(): Formulae = {
    val left:Formulae = this.getLeftFormula.nnf()
    val right:Formulae = this.getRightFormula.nnf()
    boolFormulaFactory(BooleanConnectiveType.OR, left, right)
  }

  override def negate(): Formulae = {
    this.nnf().negate()
  }

  //override def toPrincess: IFormula = this.getLeftFormula.toPrincess ==> this.getRightFormula.toPrincess
}
