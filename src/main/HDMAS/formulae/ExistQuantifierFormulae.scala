package formulae
import formulae.FormulaeType.{EXISTS, FormulaeType}
import formulae.QuantifierType.QuantifierType

class ExistQuantifierFormulae(nested:Formulae,qvar1:Variable) extends QuantifierFormulae(nested,qvar1,null) {

  //if ((qvar1 != null && qvar2 != null) || (qvar1 == null && qvar2 == null)) {throw new RuntimeException("Incorrect quantified variable")}

  override def stringQuantifier(): String = {
    "EXISTS"
  }

  override def getFormulaType: FormulaeType = EXISTS

  override def getQuantifierType: QuantifierType = {
    QuantifierType.EXISTS
  }

  override def nnf(): Formulae = {
   val nested: Formulae = this.getNestedFormula.nnf()
   val qvar1: Variable = this.getQuantifiedVariable1
   //val qvar2:Variable = this.getQuantifiedVariable1_2
   this.quantifierFormulaFactory(this.getQuantifierType, nested,qvar1,null)

  }

  override def negate(): Formulae = {
    val nested = this.getNestedFormula
    val qvar1 = this.getQuantifiedVariable1
    //val qvar2 = this.getQuantifiedVariable1_2
    this.quantifierFormulaFactory(QuantifierType.FORALL,nested,qvar1,null)
  }

  override def toString: String = this.stringQuantifier()  + this.getQuantifiedVariable1 + ": (" + this.getNestedFormula + ")"
}
