package formulae
import formulae.FormulaeType.FormulaeType

class AtomicProposition(n:String) extends AtomicFormulae {
  val name:String = n
  def getName:String = this.name

  override def getFormulaType: FormulaeType = FormulaeType.ATOMIC

  override def substitute(oldOne: Variable, newOne: Term): Unit = this

  override def toString: String = this.getName

  override def equals(obj: Any): Boolean = {
    var res:Boolean = false

    if (obj != null && this.getClass.equals(obj.getClass)){
      val other:AtomicProposition = obj.asInstanceOf[AtomicProposition]
      res = this.getName == other.name
    }
    res
  }



}
