import Model_Checking.ModelChecking
import Structure.{Action, HDMAS_Structure, State}
import ap.parser.IExpression.ConstantTerm
import ap.parser.{IConstant, IFormula, INot}
import formulae.{AtomicProposition, Constant, Formulae, GloballyFormulae, NextFormulae, NotFormulae, StrategicFormulae, UntilFormulae, Variable}

import scala.collection.mutable

object fortress {
  def main(args: Array[String]): Unit ={
    val s1:State = new State("s1")
    val s2:State = new State("s2")
    val stateSet:mutable.Set[State] = mutable.Set(s1, s2)

    val act1:Action = new Action(1)
    val act2:Action = new Action(2)
    val act3:Action = new Action(3)
    val act4:Action = new Action(4)
    val act1_bar:Action = new Action(5)
    val act2_bar:Action = new Action(6)
    val act3_bar:Action = new Action(7)
    val act4_bar:Action = new Action(8)
    val None:Action = new Action(0) //act_epsilon
    val actionPlus:mutable.Set[Action] = mutable.Set(None,act1,act2, act3, act4,act1_bar,act2_bar, act3_bar, act4_bar)

    val d:Map[State,mutable.Set[Action]] = Map((s1,actionPlus),(s2,actionPlus))

    val x1 = IConstant(act1.p)
    val x2 = IConstant(act2.p)
    val x3 = IConstant(act3.p)
    val x4 = IConstant(act4.p)
    val x1_bar = IConstant(act1_bar.p)
    val x2_bar = IConstant(act2_bar.p)
    val x3_bar = IConstant(act3_bar.p)
    val x4_bar = IConstant(act4_bar.p)
    val xMap:Map[Int,ConstantTerm] = Map((1,x1.c),(2,x2.c),(3,x3.c),(4,x4.c),(5,x1_bar.c),(6,x2_bar.c),(7,x3_bar.c),(8,x4_bar.c))

    val g1:IFormula = (x1<1 & (x1<5 & x1_bar>x1)) &  (x2<1 & (x2<5 & x2_bar>x2)) &  (x3<1 & (x3<5 & x3_bar>x3)) &  (x4<1 & (x4<5 & x4_bar>x4))
    val g2:IFormula = x1 === x1

    val deltaSS:Map[(State, State),IFormula] = Map(((s1,s1),INot(g1)),((s1,s2),g1),((s2,s2),g2))

    val captured:AtomicProposition = new AtomicProposition("captured" )

    val lambda:Map[State,mutable.Set[AtomicProposition]] = Map((s1,mutable.Set()),(s2,mutable.Set(captured)))

    val M:HDMAS_Structure = new HDMAS_Structure(actionPlus,stateSet,d,deltaSS,lambda,xMap)

    val formula:Formulae = new StrategicFormulae(new Constant(15),new Constant(20),new GloballyFormulae(new NotFormulae(captured)))

    val nf = new Normal_Form.Function
    val mc = new ModelChecking
    println(mc.GlobalMC(M,nf.NF(formula),null))
  }

}
