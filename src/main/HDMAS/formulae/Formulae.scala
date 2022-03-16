package formulae


import formulae.FormulaeType.FormulaeType

trait Formulae extends Cloneable{
  override def toString: String = super.toString

  override def equals(obj: Any): Boolean = super.equals(obj) //scala中的any对应Java中的object

  override def hashCode(): Int = super.hashCode()

  def getFormulaType: FormulaeType

  //def clone(): Formulae

  def copy(): Formulae = this.copy() //scala中clone返回值类型为AnyRef，无法改变类型，因此此处换为copy

  def nnf(): Formulae

  def negate(): Formulae

  def substitute(oldOne:Variable,newOne:Term):Unit

  //def assignSort(variable: Variable,sort: Sort):Unit



  //override def clone(): AnyRef = super.clone()

  //override def clone() = super.clone()


  //def nnf: Formula



}
