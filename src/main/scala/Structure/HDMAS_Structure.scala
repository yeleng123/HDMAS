package Structure
//import ap.SimpleAPI._
import ap.parser._
import IExpression._
import formulae.{Atomic, AtomicProposition, Predicate, Variable}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//把action sigma永远放在action set最后一位
class HDMAS_Structure(Ag:mutable.Set[Agent],ActPlus:mutable.Set[Action],S:mutable.Set[State],d:Map[State,mutable.Set[Action]],delta:Map[(State, State),IFormula],lambda:Map[State,mutable.Set[AtomicProposition]],ac:Map[Int,ConstantTerm]) {
  val agentSet:mutable.Set[Agent] = Ag
  val agNum:Int = this.agentSet.size
  val actionSet:mutable.Set[Action] = ActPlus
  val stateSet:mutable.Set[State] = S
  val aaf: Map[State,mutable.Set[Action]] = d
  val tg: Map[(State, State),IFormula] = delta
  val lf: Map[State,mutable.Set[AtomicProposition]] = lambda
  var P: Map[State,ListBuffer[ListBuffer[Int]]] = Map(stateSet.head -> ListBuffer[ListBuffer[Int]]())//action distribution
  val actionCounter:Map[Int,ConstantTerm] = ac

  def getAgentSet:mutable.Set[Agent] = this.agentSet

  def getAgNum:Int = this.agNum

  def getActionSet:mutable.Set[Action] = this.actionSet

  def getStateSet:mutable.Set[State] = this.stateSet

  def getActionCounter:Map[Int,ConstantTerm] = this.actionCounter

  def AAF(s:State):mutable.Set[Action] = {d(s)}

  def TG(s1:State,s2:State):IFormula = {delta((s1,s2))}

  def LF(s:State):mutable.Set[AtomicProposition] = {lambda(s)}

  def AD(state:State, n:Int,m:Int,l:ListBuffer[Int], idx:Int):ListBuffer[ListBuffer[Int]] = {
    var store = ListBuffer[ListBuffer[Int]]()
    if (m == 1) {
      val ll = l.clone()
      ll.update(idx,n)
      ///P(state) += l
      //return l
      //println(l)

      //val lValue = l
      //println(ListBuffer())
      ListBuffer(ll)
      //new ListBuffer[ListBuffer[Int]](l)
    } else {
      for (i <- 0 to n){
        //println(store)
        l.update(idx,i)
        //println(l)
        store ++= AD(state,n-i,m-1,l,idx+1)
        //println(store)
      }
      store
    }

  }


  def actionDistribution():Unit = {
    this.stateSet.foreach(x => {
      if (P.contains(x)){
        val l = ListBuffer.fill(this.AAF(x).size)(0)
        this.P(x) ++= AD(x,this.getAgNum,this.AAF(x).size,l,0)
      } else {
        P += (x -> ListBuffer[ListBuffer[Int]]())
        val l = ListBuffer.fill(this.AAF(x).size)(0)
        this.P(x) ++= AD(x,this.getAgNum,this.AAF(x).size,l,0)
      }
    })
  }

}

object HDMAS_Structure{
  def main(args: Array[String]): Unit = {
    /*
    val s1:State = new State("s1")
    val s2:State = new State("s2")
    val s3:State = new State("s3")
    val s4:State = new State("s4")
    val s5:State = new State("s5")
    val s6:State = new State("s6")
    val stateSet:mutable.Set[State] = mutable.Set(s1, s2, s3, s4, s5, s6)

    val agentSet:mutable.Set[Agent] = mutable.Set()
    for (i <- 1 to 10){
      agentSet.add(new Agent(s"ag$i"))
    }

    val agNum:Int = 10

    //action name
    val act1:Action = new Action(1)
    val act2:Action = new Action(2)
    val act3:Action = new Action(3)
    val None:Action = new Action(0)
    val actionPlus:mutable.Set[Action] = mutable.Set(None,act1,act2, act3)

    val d:Map[State,mutable.Set[Action]] = Map((s1,actionPlus),(s2,mutable.Set(None,act1,act3)),(s3,actionPlus),(s4,actionPlus),(s5,mutable.Set(None,act2,act3)),(s6,mutable.Set(None,act1)))

    val x1:IVariable = act1.p
    val x2:IVariable = act2.p
    val x3:IVariable = act3.p
    val g1:IFormula = (x1 >= 2 * x2) & (x3 > 3)
    val g2:IFormula = (x1 + x2 + x3 <= 10) & (x3 > 3)
    val g3:IFormula = (x1 > 5) & (x3 > x1)
    val g4:IFormula = (x1 > 5) & (3 * x2 < x1 + 2 * x3)
    val g5:IFormula = x1 === x1
    val g6:IFormula = x1 + 2 * x2 >= x3
    val g7:IFormula = x2 === x3

    val guards:Map[String,IFormula] = Map(("g1",g1),("g2",g2),("g3",g3),("g4",g4),("g5",g5),("g6",g6),("g7",g7))

    val deltaSS:Map[(State, State),IFormula] = Map(((s1,s1),INot(g1) &  INot(g2)),((s1,s2),g1),((s1,s3),g2),((s2,s3),INot(g3)),((s2,s4),g3),((s4,s3),INot(g4)),((s4,s6),g4),((s5,s2),INot(g7)),((s5,s6),g7),((s6,s6),g5))


    val aName = new AtomicProposition("name")
    val aAge = new AtomicProposition("age")


    val lambda:Map[State,mutable.Set[AtomicProposition]] = Map((s1,mutable.Set(aName)),(s2,mutable.Set(aAge)),(s3,mutable.Set(aName)),(s4,mutable.Set(aAge)),(s5,mutable.Set(aName)),(s6,mutable.Set(aAge)))

    val H:HDMAS_Structure = new HDMAS_Structure(agentSet,actionPlus,stateSet,d,deltaSS,lambda)
    //println(H.AD(s1,4,3,ListBuffer.fill(3)(0),0))
    H.actionDistribution()
    println(H.P(s5))
    //println(H.P)
    //action counters
    /* val x1:IVariable = x1
    val x2:IVariable = x2
    val x3:IVariable = x3
    val n:IVariable = n
    val g1:IFormula = (x1 >= 2 * x2) & (x3 > 3)
    val g2:IFormula = (x1 + x2 + x3 <= 10) & (x3 > 3)
    val g3:IFormula = (x1 > 5) & (x3 > x1)
    val g4:IFormula = (x1 > 5) & (3 * x2 < x1 + 2 * x3)
    val g5:IFormula = x1 === x1
    val g6:IFormula = x1 + 2 * x2 >= x3
    val g7:IFormula = x2 === x3

    val guards:Map[String,IFormula] = Map(("g1",g1),("g2",g2),("g3",g3),("g4",g4),("g5",g5),("g6",g6),("g7",g7))

    val deltaSS:Map[(State, State),IFormula] = Map(((s1,s1),INot(g1) &  INot(g2)),((s1,s2),g1),((s1,s3),g2),((s2,s3),INot(g3)),((s2,s4),g3),((s4,s3),INot(g4)),((s4,s6),g4),((s5,s2),INot(g7)),((s5,s6),g7),((s6,s6),g5))


     */

     */



  }
}

