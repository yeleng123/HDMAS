package Structure
import scala.collection.mutable

class State(name:String) extends Cloneable{
  var state:String = name

  def getState:String = this.state

  def setState(newAction:String):Unit = {this.state = newAction}

  override def toString: String = this.state


}
