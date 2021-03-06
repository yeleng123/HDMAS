package formulae

import formulae.FormulaeType.{FormulaeType, STRATEGIC}

//import scala.beans.BeanProperty

class StrategicFormulae(t1:Term,t2:Term,nested:PathFormulae) extends Formulae{
  var controllableAgent:Term = t1
  var uncontrollableAgent:Term = t2
  var nestedFormula:PathFormulae = nested
  val y1:Variable = new Variable(1)
  val y2:Variable = new Variable(2)
  if (this.getControllableAgent == y2 || this.getUncontrollableAgent == y1){ throw new RuntimeException("Incorrect strategic formula")}



  def getControllableAgent : Term = this.controllableAgent
  def getUncontrollableAgent : Term = this.uncontrollableAgent
  def getNestedFormula:PathFormulae = nestedFormula

  def setControllableAgent(ca:Term):Unit = {this.controllableAgent = ca}
  def setUncontrollableAgent(uca:Term):Unit = {this.uncontrollableAgent = uca}

  override def getFormulaType: FormulaeType = STRATEGIC

  override def toString: String = "《" + this.getControllableAgent + ", " + this.getUncontrollableAgent +"》" + "[" +this.nestedFormula.toString + "]"

  override def copy(): Formulae = {
    new StrategicFormulae(this.getControllableAgent, this.getUncontrollableAgent, this.getNestedFormula.copy().asInstanceOf[PathFormulae])
  }

  override def equals(obj:Any): Boolean = {
    var res = false

    if (this.getClass.equals(obj.getClass)){
      val other:StrategicFormulae = obj.asInstanceOf[StrategicFormulae]
      res = this.getControllableAgent ==other.getControllableAgent && this.getUncontrollableAgent == other.getUncontrollableAgent && this.getNestedFormula.equals(other.getNestedFormula)
    }
    res
  }

  override def hashCode(): Int = {
    var res:Int = this.getClass.hashCode()
    res = res * 31
    res = res + (if(this.getControllableAgent.asInstanceOf[Int] < 0) this.getControllableAgent.hashCode() else 0)
    res = res * 31
    res = res + (if(this.getUncontrollableAgent.asInstanceOf[Int] < 0) this.getUncontrollableAgent.hashCode() else 0)
    res = res * 31
    res = res + (if(this.getNestedFormula != null) this.getNestedFormula.hashCode() else 0)
    res
  }

  override def substitute(oldOne: Variable, newOne: Term): Unit = {
    if (this.getControllableAgent == oldOne){this.controllableAgent = newOne}
    if (this.getUncontrollableAgent == oldOne){this.uncontrollableAgent = newOne}
    this.nestedFormula.substitute(oldOne,newOne)
  }

  override def nnf(): Formulae = {
    new StrategicFormulae(this.getControllableAgent, this.getUncontrollableAgent, this.getNestedFormula.nnf().asInstanceOf[PathFormulae])
  }

  override def negate(): Formulae = null //???
}
