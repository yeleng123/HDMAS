package formulae
import ap.parser.IFormula
import formulae.BooleanConnectiveType.{AND, BooleanConnectiveType}
import formulae.FormulaeType.FormulaeType

class AndFormulae(left:Formulae, right:Formulae) extends BinaryFormulae(left,right) with BooleanOpFormulae {//Scala为单继承

  override def stringOperator(): String = {
    "AND"
  }

  override def getOpType: BooleanConnectiveType = AND

  override def getFormulaType: FormulaeType = {
    FormulaeType.AND
  }

  override def nnf(): formulae.Formulae = {
    val left:formulae.Formulae = this.getLeftFormula.nnf()
    val right:formulae.Formulae = this.getRightFormula.nnf()
    this.boolFormulaFactory(this.getOpType, left, right)
  }

  override def negate(): Formulae = {
    val left:formulae.Formulae = this.getLeftFormula.negate()
    val right:formulae.Formulae = this.getRightFormula.negate()
    this.boolFormulaFactory(BooleanConnectiveType.OR, left, right)
  }

  /* def toPrincess: IFormula = {
    this.getLeftFormula.toPrincess & this.getRightFormula.toPrincess
  } */


}

object AndFormulae{
  def main(args: Array[String]): Unit ={
    val name = new Variable(1)
    val age = new Variable(2)
    val pName = new Predicate("Name",1)
    val qAge = new Predicate("Age",1)
    val aName = new Atomic(pName,name)
    val aAge = new Atomic(qAge,age)
    val and = new AndFormulae(aName,aAge)
    val newName = new Constant(1)
    and.substitute(name,newName)
    println(and.toString)
  }

}
