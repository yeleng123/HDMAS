package formulae

import formulae.FormulaeType.{EQUALITY, FormulaeType}

//import scala.beans.BeanProperty

class EqualityFormulae(left:Term, right:Term) extends AtomicFormulae {
  var leftTerm:Term = left
  var rightTerm:Term = right

  def getLeftTerm:Term = this.leftTerm
  def getRightTerm:Term = this.rightTerm

  override def toString: String = {
    this.getLeftTerm.toString + " == " + this.getRightTerm.toString
  }

  override def hashCode(): Int = {
    var res:Int = if (this.getLeftTerm != null) this.getLeftTerm.hashCode() else 0
    res = res * 31
    res = res + (if (this.getRightTerm != null) this.getRightTerm.hashCode() else  0)
    res
  }

  override def equals(obj: Any): Boolean = {
    var res:Boolean = false

    if (obj != null && this.getClass.equals(obj.getClass)) {
      val other:EqualityFormulae = obj.asInstanceOf[EqualityFormulae]
      res = this.getLeftTerm.equals(other.getLeftTerm) && this.getRightTerm.equals(other.getRightTerm)
    }
    res
  }

  override def getFormulaType: FormulaeType = EQUALITY

  override def copy(): Formulae = this.formulaFactory(this.getFormulaType,this.getLeftTerm.copy(),this.getRightTerm.copy(), null, null)

  override def substitute(oldOne: Variable, newOne: Term): Unit = {
    this.leftTerm.substitute(oldOne, newOne)
    this.rightTerm.substitute(oldOne,newOne)
  }




}
