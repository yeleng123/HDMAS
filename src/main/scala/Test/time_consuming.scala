package Test

import formulae._
import Structure._
import Normal_Form._
import Model_Checking.ModelChecking
import ap.parser.IExpression._
import ap.parser._
import ap.SimpleAPI._

import scala.collection.mutable.ArrayBuffer
//import java.util.Date

import scala.collection.mutable

object time_consuming {
  def main(args: Array[String]): Unit ={
    val y1: Variable = new Variable(1)
    val y2: Variable = new Variable(2)

    val p1 = new AtomicProposition("p1")
    val p2 = new AtomicProposition("p2")

    val act1:Action = new Action(1)
    val act2:Action = new Action(2)
    val act3:Action = new Action(3)
    val None:Action = new Action(0) //act_epsilon
    val actionPlus:mutable.Set[Action] = mutable.Set(None,act1,act2,act3)

    val x1 = IConstant(act1.p)
    val x2 = IConstant(act2.p)
    val x3 = IConstant(act3.p)
    val xMap:Map[Int,ConstantTerm] = Map((1,x1.c),(2,x2.c),(3,x3.c))


    val allTime:ArrayBuffer[Long] = new ArrayBuffer[Long](20)

    for(i <- 0 to 99){
      if((i+1)%5 == 0){
        //guards
        val g1:IFormula = (x1 +x2 +x3 >= 10) & (x1- 2*x2 <= 3)
        val g2:IFormula = x1 === x1

        // 创建100个state，并把它们一起保存在集合StateSet中
        val StateSet:ArrayBuffer[State] = new ArrayBuffer[State](100)
        for(i <- 1 to 100){
          StateSet += new State(s"s${i}")
        }


        //创建d
        val d:ArrayBuffer[Map[State,mutable.Set[Action]]]= new ArrayBuffer[Map[State, mutable.Set[Action]]](100)
        d += Map((StateSet(0),actionPlus))
        for(i <- 1 to 99){
          var sub_d:Map[State,mutable.Set[Action]] = d(i-1)
          sub_d += ((StateSet(i),actionPlus))
          d += sub_d
        }

        //创建lambda
        val lambda:ArrayBuffer[Map[State,mutable.Set[AtomicProposition]]] = new ArrayBuffer[Map[State, mutable.Set[AtomicProposition]]](100)
        lambda += Map((StateSet(0),mutable.Set(p1,p2)))
        for(i <- 1 to 99){
          var sub_lambda:Map[State,mutable.Set[AtomicProposition]] = lambda(i-1)
          sub_lambda += ((StateSet(i),mutable.Set(p1,p2)))
          lambda += sub_lambda
        }

        //创建delta
        val delta:ArrayBuffer[Map[(State, State),IFormula]] = new ArrayBuffer[Map[(State, State), IFormula]](100)
        delta += Map(((StateSet(0),StateSet(0)),g2))
        for(i <- 1 to 99){
          var sub_delta:Map[(State, State),IFormula] = delta(i-1)
          sub_delta = sub_delta.dropRight(1)
          sub_delta ++= Map(((StateSet(i-1),StateSet(i)),g1),((StateSet(i-1),StateSet(i-1)),INot(g1)),((StateSet(i),StateSet(i)),g2))
          delta += sub_delta
        }

        //创建HDMAS
        val HDMAS:ArrayBuffer[HDMAS_Structure] = new ArrayBuffer[HDMAS_Structure](100)
        for(i <- 1 to 100){
          val StateArray:ArrayBuffer[State] = StateSet.take(i)
          var set:mutable.Set[State] = mutable.Set[State]()
          StateArray.foreach(x => set += x)

          HDMAS += new HDMAS_Structure(actionPlus,set,d(i-1),delta(i-1),lambda(i-1),xMap)
        }


        val formula:Formulae = new StrategicFormulae(new Constant(10),new Constant(3),new NextFormulae(new AndFormulae(p1,p2)))

        val f = new Function
        val NF_formula = f.NF(formula)

        val startTime:Long = System.currentTimeMillis()
        val mc = new ModelChecking
        mc.GlobalMC(HDMAS(i),NF_formula,null)
        val endTime:Long = System.currentTimeMillis()
        val t:Long = endTime - startTime
        allTime += t

      }
    }
    println(allTime)



    /*
    //guards
    val g1:IFormula = (x1 +x2 +x3 >= 10) & (x1- 2*x2 <= 3)
    val g2:IFormula = x1 === x1

    // 创建100个state，并把它们一起保存在集合StateSet中
    val StateSet:ArrayBuffer[State] = new ArrayBuffer[State](100)
    for(i <- 1 to 100){
      StateSet += new State(s"s${i}")
    }


    //创建d
    val d:ArrayBuffer[Map[State,mutable.Set[Action]]]= new ArrayBuffer[Map[State, mutable.Set[Action]]](100)
    d += Map((StateSet(0),actionPlus))
    for(i <- 1 to 99){
      var sub_d:Map[State,mutable.Set[Action]] = d(i-1)
      sub_d += ((StateSet(i),actionPlus))
      d += sub_d
    }

    //创建lambda
    val lambda:ArrayBuffer[Map[State,mutable.Set[AtomicProposition]]] = new ArrayBuffer[Map[State, mutable.Set[AtomicProposition]]](100)
    lambda += Map((StateSet(0),mutable.Set(p1,p2)))
    for(i <- 1 to 99){
      var sub_lambda:Map[State,mutable.Set[AtomicProposition]] = lambda(i-1)
      sub_lambda += ((StateSet(i),mutable.Set(p1,p2)))
      lambda += sub_lambda
    }

    //创建delta
    val delta:ArrayBuffer[Map[(State, State),IFormula]] = new ArrayBuffer[Map[(State, State), IFormula]](100)
    delta += Map(((StateSet(0),StateSet(0)),g2))
    for(i <- 1 to 99){
      var sub_delta:Map[(State, State),IFormula] = delta(i-1)
      sub_delta = sub_delta.dropRight(1)
      sub_delta ++= Map(((StateSet(i-1),StateSet(i)),g1),((StateSet(i-1),StateSet(i-1)),INot(g1)),((StateSet(i),StateSet(i)),g2))
      delta += sub_delta
    }

    //创建HDMAS
    val HDMAS:ArrayBuffer[HDMAS_Structure] = new ArrayBuffer[HDMAS_Structure](100)
    for(i <- 1 to 100){
      val StateArray:ArrayBuffer[State] = StateSet.take(i)
      var set:mutable.Set[State] = mutable.Set[State]()
      StateArray.foreach(x => set += x)

      HDMAS += new HDMAS_Structure(actionPlus,set,d(i-1),delta(i-1),lambda(i-1),xMap)
    }


    val formula:Formulae = new StrategicFormulae(new Constant(10),new Constant(3),new NextFormulae(new AndFormulae(p1,p2)))

    val f = new Function
    val NF_formula = f.NF(formula)

    val allTime:ArrayBuffer[Long] = new ArrayBuffer[Long](100)



    val startTime:Long = System.currentTimeMillis()
    val mc = new ModelChecking
    mc.GlobalMC(HDMAS(94),NF_formula,null)
    val endTime:Long = System.currentTimeMillis()
    println(endTime-startTime)




     */




  }

}
