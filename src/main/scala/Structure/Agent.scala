package Structure

import scala.collection.mutable
//import scala.collection.mutable.Set

class Agent(name:String) extends Cloneable{
  var agent:String = name

  def getAgent : String = this.agent

  def setAgent(newagent:String) : Unit = {this.agent = newagent}


}
