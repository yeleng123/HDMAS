package formulae
//import scala.beans.BeanProperty

import ap.parser._
import ap.terfor.preds
import IExpression._
class Predicate(n:String,a:Int) extends Cloneable{ // 谓词：表关系

  //@BeanProperty var name:String = n
  //@BeanProperty var arity:Int = a
  var name:String = n
  var arity:Int = a

  def getName:String = this.name

  def getArity:Int = this.arity

  override def toString: String = this.getName

  override def equals(obj: Any): Boolean = {
    var res:Boolean = false

    if (obj != null && this.getClass.equals(obj.getClass)) {
      val other:Predicate = obj.asInstanceOf[Predicate]
      res = this.getName.equals(other.getName) && this.getArity == other.getArity
    }
    res
  }

  override def hashCode(): Int = {
    var res:Int = if(this.getName != null) this.getName.hashCode else 0
    res = res * 31
    res = res + this.getArity
    res = res * 31
    res
  }

  def copy():Predicate = new Predicate(this.getName, this.getArity)

  def toPrincess = new ap.terfor.preds.Predicate(this.getName,getArity)



}
