package formulae
import formulae.FormulaeType.{FormulaeType, NEXT}
import formulae.OperatorType.OperatorType

class NextFormulae(nested:Formulae) extends PathFormulae(null,nested){


  override def getFormulaType: FormulaeType = NEXT

  override def getTempOperatorType: OperatorType = OperatorType.NEXT

  override def stringTempOperator: String = "X "

  override def nnf(): Formulae = new NextFormulae(this.getRightFormula.nnf())

  override def negate(): Formulae = null //???

}
