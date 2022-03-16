package formulae
import java.util
import java.util.HashSet
import java.util.Iterator
//import scala.beans.BeanProperty

class Sort(n:String) extends util.LinkedHashSet[Constant] with Cloneable{ //Scala中hashset为一个final class 无法extend 因此直接调用Java中的hashset

  //val this.nameValue = name
  //@ BeanProperty private var name:String = n
  private var name:String = n
  private var variables:util.LinkedHashSet[Variable] = new util.LinkedHashSet()
  this.setName(name)

  //def setName(name:String):Unit = {this.name = name} //Unit对应java中的void，表示没有返回值

  def getName:String = this.name
  def setName(str: String):Unit = {this.name = str}

  def addVariable(v:Variable):Unit = this.variables.add(v)

  def getVariables:util.LinkedHashSet[Variable] = this.variables

  override def toString: String = {
    "SORT" + this.getName + "::" + super.toString + "::" + this.getVariables.toString
  }

  override def equals(obj: Any): Boolean = {
    var res:Boolean = false
    if (obj.isInstanceOf[Sort]) {
      val other:Sort = obj.asInstanceOf[Sort]
      res = this.getName.equals(other.getName) && super.equals(obj)
    }
    res
  }

  override def hashCode(): Int = {
    var res:Int = if (this.getName != null) this.getName.hashCode else 0
    res = res*31
    res = res + super.hashCode()
    res
  }

  def copy(): Sort = {
    val res:Sort = new Sort(this.getName)
    val i:Iterator[Constant] = this.iterator()
    val vars:Iterator[Variable] = this.variables.iterator()

    while (i.hasNext) {
      val c :Constant = i.next()
      res.add(c)
    }

    while (vars.hasNext) {
      val v:Variable = vars.next()
      res.addVariable(v)
    }
    res
  }

}

object Sort{
  final var DEFAULT:Sort = new Sort("default")
}
