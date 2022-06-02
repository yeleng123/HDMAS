package Structure
import ap.parser.IVariable
import ap.terfor.ConstantTerm
import ap.types._
import ap.types.Sort.Nat

import scala.collection.mutable
//由于IVariable中的参数为Int，无法输入String，因此此处action的参数设为Int
class Action(name:Int) extends Cloneable{
  var action:Int= name

  def getAction:Int= this.action

  def setAction(newAction:Int):Unit = {this.action = newAction}

  //action profile
  val p:ConstantTerm = new SortedConstantTerm(s"x_$name",Nat)




}
