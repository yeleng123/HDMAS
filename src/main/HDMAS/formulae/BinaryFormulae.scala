package formulae

import formulae.FormulaeType. FormulaeType
//import scala.beans.BeanProperty

abstract class BinaryFormulae(left:Formulae,right:Formulae) extends Formulae {
  //private var left:Formulae = _
  //private var right:Formulae = _

  /* def binary_Formulae(left:Formulae,right:Formulae):Unit = {
    this.left = left
    this.right = right
  } */
  var leftFormula:Formulae = left
  var rightFormula:Formulae = right

  def getLeftFormula: Formulae = this.leftFormula

  def  getRightFormula: Formulae = this.rightFormula

  //override def substitute(assignment: Assignment): Formulae = this.formulaFactory(this.getFormulaType,this.getLeftFormula.substitute(assignment),this.getRightFormula.substitute(assignment))

  /* override def assignSort(variable: Variable, sort: Sort): Unit = {
    this.getLeftFormula.assignSort(variable,sort)
    this.getRightFormula.assignSort(variable,sort)
  } */

  override def toString: String = {
    "(" + this.getLeftFormula + ")" + this.stringOperator() + "(" + this.getRightFormula + ")"
  }

  //java条件运算符： variable x = (expression) ? value if true : value if false
  //scala条件运算： variable x = if (expression) value if true else value if false
  override def hashCode(): Int = {
    var res:Int  = 0
    res = if (this.getLeftFormula != null)  this.getLeftFormula.hashCode() else 0
    res =31*res
    res = res + (if (this.getRightFormula != null) this.getRightFormula.hashCode() else 0)
    res
  }

  override def equals(o: Any): Boolean = {
    var res:Boolean = false

    if (o != null && this.getClass.equals(o.getClass)){
      val other:BinaryFormulae = o.asInstanceOf[BinaryFormulae] //向下转换，any类转化为更具体的类
      res = this.getLeftFormula.equals(other.getLeftFormula) && this.getRightFormula.equals(other.getRightFormula)
    }

    res
  }

  def formulaFactory(formulaeType:FormulaeType, left:Formulae, right:Formulae): Formulae = formulaeType match {
    case FormulaeType.DOUBLE_IMPL => new DoubleImplFormulae(left,right)
    case FormulaeType.IMPLICATION => new ImpFormulae(left,right)
    case FormulaeType.AND => new AndFormulae(left,right)
    case FormulaeType.OR => new OrFormulae(left,right)
    case FormulaeType.UNTIL => new UntilFormulae(left, right)
    case _ => throw new RuntimeException("Unknown formula type")
  }

  override def copy(): Formulae = {
    this.formulaFactory(this.getFormulaType,this.getLeftFormula.copy(),this.getRightFormula.copy())
  }

  override def substitute(oldOne: Variable, newOne: Term): Unit = {
    this.leftFormula.substitute(oldOne, newOne)
    this.rightFormula.substitute(oldOne, newOne)
  }

  def stringOperator(): String

}
