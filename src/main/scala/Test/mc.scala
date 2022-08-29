package Test

import formulae._
import Structure._
import Normal_Form._
import Model_Checking.ModelChecking
import ap.parser.IExpression._
import ap.parser._
import ap.SimpleAPI._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

object mc {
  def main(args: Array[String]): Unit = {
    //set 9 states
    val s1:State = new State("s1")
    val s2:State = new State("s2")
    val s3:State = new State("s3")
    val s4:State = new State("s4")
    val s5:State = new State("s5")
    val s6:State = new State("s6")
    val s7:State = new State("s7")
    val s8:State = new State("s8")
    val s9:State = new State("s9")
    val stateSet:mutable.Set[State] = mutable.Set(s1, s2, s3, s4, s5, s6, s7, s8, s9)
    //println(stateSet)


    //action set
    val act1:Action = new Action(1)
    val act2:Action = new Action(2)
    val act3:Action = new Action(3)
    val act4:Action = new Action(4)
    val None:Action = new Action(0) //act_epsilon
    val actionPlus:mutable.Set[Action] = mutable.Set(None,act1,act2, act3, act4)

    //action availability function
    val d:Map[State,mutable.Set[Action]] = Map((s1,actionPlus),(s2,mutable.Set(None,act2,act3)),
      (s3,mutable.Set(None,act1,act3,act4)),(s4,actionPlus),(s5,actionPlus),(s6,mutable.Set(None,act1,act2,act3)),
      (s7,mutable.Set(None,act1,act3)),(s8,mutable.Set(None,act2,act3)),(s9,mutable.Set(None,act1)))

    //action counter
    val x1 = IConstant(act1.p)
    val x2 = IConstant(act2.p)
    val x3 = IConstant(act3.p)
    val x4 = IConstant(act4.p)
    val xMap:Map[Int,ConstantTerm] = Map((1,x1.c),(2,x2.c),(3,x3.c),(4,x4.c))

    //guards
    val g1:IFormula = (x1 + x2 >= x3) & (x4 >= 4)
    val g2:IFormula = (x2 + 2*x3 >= x1 + x4) & (x4 < 4)
    val g3:IFormula = (x2 > 4) & (x2 > x3)
    val g4:IFormula = (x2 + x3 > 6) & (x3 > 4)
    val g5:IFormula = (x1 + x4 <= 10) & (x1 + x2 + x3 + x4 > 20)
    val g6:IFormula = (x1 > 5) & (x1 + 2*x3 > 3*x4)
    val g7:IFormula = x1 + 2*x3 > x4
    val g8:IFormula = (x1 > x4) & (x2 > 2*x3)
    val g9:IFormula = (x1 >= 2*x2) & (x3 + x4 >= 10)
    val g10:IFormula = (x1 + x2 + x3 <= 10) & (x3 >3)
    val g11:IFormula = (x1 > 5) & (x3 > x1)
    val g12:IFormula = 2*x2 === x3
    val g13:IFormula = x1 === x1

    //transition guard function
    val deltaSS:Map[(State, State),IFormula] = Map(((s1,s1),INot(g1)&INot(g2)),((s1,s2),g1),((s1,s4),g2),((s2,s3),g3),
      ((s2,s5),g4),((s2,s4),INot(g3)&INot(g4)),((s3,s6),g6),((s3,s5),g7),((s3,s7),INot(g6)&INot(g7)),((s4,s2),g5),
      ((s4,s5),INot(g5)), ((s5,s4),g9),((s5,s7),g8),((s5,s8),INot(g8)&INot(g9)),((s6,s9),g10),((s6,s7),INot(g10)),
      ((s7,s9),g11),((s7,s8),INot(g11)),((s8,s9),g12),((s8,s5),INot(g12)),((s9,s9),g13))

    //atomic proposition
    val p1:AtomicProposition = new AtomicProposition("p1" )
    val p2:AtomicProposition = new AtomicProposition("p2" )
    val p3:AtomicProposition = new AtomicProposition("p3" )

    //labelling function
    val lambda:Map[State,mutable.Set[AtomicProposition]] = Map((s1,mutable.Set(p1)),(s2,mutable.Set(p2)),(s3,mutable.Set(p1,p3)),
      (s4,mutable.Set(p1,p2)),(s5,mutable.Set(p3)),(s6,mutable.Set()),(s7,mutable.Set(p2)),(s8,mutable.Set(p1,p2,p3)),
      (s9,mutable.Set(p3)))

    //create the HDMAS structure
    val M:HDMAS_Structure = new HDMAS_Structure(actionPlus,stateSet,d,deltaSS,lambda,xMap)


    val y1:Variable = new Variable(1)
    val y2:Variable = new Variable(2)
    val phi1:Formulae = p2
    val phi2:Formulae = new NotFormulae(p1)
    val phi3:Formulae  = new AndFormulae(p1,p2)
    val phi4:Formulae = new OrFormulae(p2,p3)
    val phi5:Formulae = new StrategicFormulae(new Constant(10),new Constant(4),new NextFormulae(p3))
    val phi6:Formulae = new StrategicFormulae(new Constant(10),new Constant(4),new GloballyFormulae(p3))
    val phi7:Formulae = new StrategicFormulae(new Constant(10),new Constant(4),
      new UntilFormulae(p1,p3))
    val phi8:Formulae = new ForallQuantifierFormulae(new StrategicFormulae(new Constant(10),y2,new NextFormulae(p3)),y2)
    val phi9:Formulae = new ExistQuantifierFormulae(new StrategicFormulae(y1,new Constant(4),new GloballyFormulae(p1)),y1)
    val phi10:Formulae = new StrategicFormulae(new Constant(10), new Constant(4),
      new NextFormulae(new ForallQuantifierFormulae(new StrategicFormulae(new Constant(8),
        y2,new GloballyFormulae(p3)),y2)))
    val mc = new ModelChecking

    println("The model checking result of phi_1: " + mc.GlobalMC(M,phi1,null)+ "\n")
    println("The model checking result of phi_2: " + mc.GlobalMC(M,phi2,null)+ "\n")
    println("The model checking result of phi_3: " + mc.GlobalMC(M,phi3,null)+ "\n")
    println("The model checking result of phi_4: " + mc.GlobalMC(M,phi4,null)+ "\n")
    println("The model checking result of phi_5: " + mc.GlobalMC(M,phi5,null)+ "\n")
    println("The model checking result of phi_6: " + mc.GlobalMC(M,phi6,null)+ "\n")
    println("The model checking result of phi_7: " + mc.GlobalMC(M,phi7,null)+ "\n")
    println("The model checking result of phi_8: " + mc.GlobalMC(M,phi8,null)+ "\n")
    println("The model checking result of phi_9: " + mc.GlobalMC(M,phi9,null)+ "\n")
    println("The model checking result of phi_10: " + mc.GlobalMC(M,phi10,null)+ "\n")




  }



}
