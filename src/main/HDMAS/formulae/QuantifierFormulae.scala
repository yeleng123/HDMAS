package formulae


//import formulae.FormulaeType.FormulaeType
import formulae.QuantifierType.{EE, EF, EXISTS, FE, FF, FORALL, QuantifierType}

//import scala.beans.BeanProperty


abstract class QuantifierFormulae(nf:Formulae, qv1:Variable,qv2:Variable) extends Formulae {
  //@BeanProperty var nestedFormula:Formulae = nf
  //@BeanProperty var quantifiedVariable:Variable = qv
  val nestedFormula:Formulae = nf
  val quantifiedVariable1:Variable = qv1
  val quantifiedVariable2:Variable = qv2
  val y1:Variable = new Variable("y1")
  val y2:Variable = new Variable("y2")
  //检查quantified variable一定为y1 或 y2
  if (this.getQuantifiedVariable2 == null) {
    if (this.getQuantifiedVariable1 != y1 && this.getQuantifiedVariable1 != y2){throw new RuntimeException("Incorrect Exists/Forall formula")}
  } else {
    if ((this.getQuantifiedVariable1 != y1 && this.getQuantifiedVariable1 != y2) || (this.getQuantifiedVariable2 != y1 && this.getQuantifiedVariable2 != y2)) {
      throw new RuntimeException("Incorrect FF/EE/EF/FE formula")
    }
  }
  //检查quantified variable不相等
  if (this.getQuantifiedVariable1 == this.getQuantifiedVariable2) {
    throw new RuntimeException("Incorrect quantifier formula")
  }

  def getNestedFormula:Formulae = this.nestedFormula
  def getQuantifiedVariable1:Variable = this.quantifiedVariable1
  def getQuantifiedVariable2:Variable = this.quantifiedVariable2
  def getY1: Variable = this.y1
  def getY2:Variable = this.y2

  override def hashCode(): Int = {
    var res:Int = this.getClass.hashCode() //确定是for all 还是 exists
    res = res * 31
    res = res + (if (this.getQuantifiedVariable1 != null) this.getQuantifiedVariable1.hashCode() else 0)
    res = res * 31
    res = res + (if (this.getQuantifiedVariable2 != null) this.getQuantifiedVariable2.hashCode() else 0)
    res = res * 31
    res = res + (if (this.getNestedFormula != null) this.getNestedFormula.hashCode() else 0)
    res
  }

  def stringQuantifier(): String

  def getQuantifierType: QuantifierType

  override def equals(obj: Any): Boolean = {
    var res:Boolean = false

    if (obj != null && this.getClass.equals(obj.getClass)) {
      val other:QuantifierFormulae = obj.asInstanceOf[QuantifierFormulae]
      res = this.stringQuantifier().equals(other.stringQuantifier()) && this.getQuantifiedVariable1.equals(other.getQuantifiedVariable1) && this.getQuantifiedVariable2.equals(other.getQuantifiedVariable2) && this.getNestedFormula.equals(other.getNestedFormula)
    }
    res
  }

  override def substitute(oldOne: Variable, newOne: Term): Unit = {
    this.nestedFormula.substitute(oldOne,newOne)
  }

  def quantifierFormulaFactory(quantifier:QuantifierType,nested:Formulae,var1:Variable,var2:Variable):Formulae = quantifier match {
    case FORALL => new ForallQuantifierFormulae(nested,var1)
    case EXISTS => new ExistQuantifierFormulae(nested,var1)
    case EE => new EEQuantifierFormulae(nested,var1,var2)
    case FF => new FFQuantifierFormulae(nested,var1,var2)
    case EF => new EFQuantifierFormulae(nested,var1,var2)
    case FE => new FEQuantifierFormulae(nested,var1,var2)
    case _ => throw new RuntimeException("Unknown formula type")
  }


}


