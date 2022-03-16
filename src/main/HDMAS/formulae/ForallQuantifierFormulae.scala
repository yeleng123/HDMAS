package formulae
import formulae.FormulaeType.{FORALL, FormulaeType}
import formulae.QuantifierType.QuantifierType

class ForallQuantifierFormulae(nested:Formulae, qvar1:Variable) extends QuantifierFormulae(nested,qvar1,null) {
  //if ((qvar1 != null && qvar2 != null) || (qvar1 == null && qvar2 == null)) {throw new RuntimeException("Incorrect quantified variable")}

  override def stringQuantifier(): String = {
    "FORALL"
  }

  override def getFormulaType: FormulaeType = FORALL

  override def getQuantifierType: QuantifierType = {
    QuantifierType.FORALL
  }

  override def nnf(): Formulae = {
    val nested: Formulae = this.getNestedFormula.nnf()
    val var1: Variable = this.getQuantifiedVariable1
    //val var2: Variable = this.getQuantifiedVariable1_2
    this.quantifierFormulaFactory(this.getQuantifierType, nested,var1,null)
  }

  override def negate(): Formulae = {
    val nested = this.getNestedFormula
    val var1 = this.getQuantifiedVariable1
    //val var2: Variable = this.getQuantifiedVariable1_2
    this.quantifierFormulaFactory(QuantifierType.EXISTS,nested,var1,null)
  }

  override def toString: String =  this.stringQuantifier()  + this.getQuantifiedVariable1 + ": (" + this.getNestedFormula + ")"

}
