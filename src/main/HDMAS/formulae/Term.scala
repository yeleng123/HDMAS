package formulae

import formulae.FormulaeType.{CONSTANT, FormulaeType, VARIABLE}
//import scala.beans.BeanProperty

abstract class Term(n:Any) extends Cloneable{

  //private var name:String = _

  /*def Term(name:String):Unit = {
    this.name = name
  }*/
  //@BeanProperty val name = n
  val name:Any = n

  def getName:Any = this.name

  def substitute(oldOne:Variable,newOne:Term):Term = {
    if (oldOne == this) {
      newOne
    } else oldOne
  }

  override def toString: String = this.getName.toString

  //equals比较值是否相同
  //eq比较引用是否相同
  override def equals(obj: Any): Boolean = {
    var res:Boolean = false

    if (obj != null && this.getClass.equals(obj.getClass)){
      val other:Term = obj.asInstanceOf[Term]
      res = this.getName.equals(other.name)
    }
    res
  }

  override def hashCode(): Int = {
    var res:Int = this.getClass.hashCode()
    res = 31*res
    res = res + (if (this.getName != null) this.getName.hashCode() else 0)
    res
  }

  def getTermType: FormulaeType

  def copy(): Term = {termFactory(this.getTermType,this.getName)}

  def termFactory(formulae_Type: FormulaeType.FormulaeType, name: Any):Term = formulae_Type match {
    case CONSTANT =>new Constant(name)
    case VARIABLE => new Variable(name.asInstanceOf[String])
    case _ => throw new RuntimeException("Unknown term type")
  }

}
