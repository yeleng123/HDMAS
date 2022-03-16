package formulae

import formulae.FormulaeType.FormulaeType
import formulae.QuantifierType.QuantifierType

class FEQuantifierFormulae(nested:Formulae,var1:Variable,var2:Variable) extends QuantifierFormulae(nested,var1,var2){
  //if (((var1_1 != null && var1_2!= null) || (var1_1 == null && var1_2 == null)) && ((var2_1 != null && var2_2!= null) || (var2_1 == null && var2_2 == null))) {throw new RuntimeException("Incorrect quantified variable")}
  override def nnf(): Formulae = {
    val n:Formulae = this.getNestedFormula.nnf()
    val v1:Variable = this.getQuantifiedVariable1
    //val v1_2:Variable = this.getQuantifiedVariable1_2
    //val v2_1:Variable = this.getQuantifiedVariable2_1
    val v2:Variable = this.getQuantifiedVariable2
    this.quantifierFormulaFactory(this.getQuantifierType, n, v1,v2)
  }

  override def negate(): Formulae = {
    val n:Formulae = this.getNestedFormula.nnf()
    val v1:Variable = this.getQuantifiedVariable1
    //val v1_2:Variable = this.getQuantifiedVariable1_2
    //val v2_1:Variable = this.getQuantifiedVariable2_1
    val v2:Variable = this.getQuantifiedVariable2
    this.quantifierFormulaFactory(QuantifierType.EF, n, v1,v2)
  }

  override def stringQuantifier(): String = "FE"

  override def getFormulaType: FormulaeType = FormulaeType.FE

  override def getQuantifierType: QuantifierType = QuantifierType.FE

  override def toString: String = "Forall"  + this.getQuantifiedVariable1 + "Exists"  + this.getQuantifiedVariable2 + ": (" + this.getNestedFormula + ")"

}
