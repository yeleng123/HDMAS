import Structure.{Action, Agent, HDMAS_Structure, State}
import ap.parser.IExpression.{ConstantTerm, Quantifier}
import ap.parser.{IConstant, IFormula, INot}
import formulae._

import scala.collection.mutable

object Example {
  def main(args: Array[String]): Unit ={

    //example_2
    val s1:State = new State("s1")
    val s2:State = new State("s2")
    val s3:State = new State("s3")
    val s4:State = new State("s4")
    val s5:State = new State("s5")
    val s6:State = new State("s6")
    val stateSet:mutable.Set[State] = mutable.Set(s1, s2, s3, s4, s5, s6)

    val agentSet:mutable.Set[Agent] = mutable.Set()
    for (i <- 1 to 11){
      agentSet.add(new Agent(s"ag$i"))
    }


    //action name
    val act1:Action = new Action(1)
    val act2:Action = new Action(2)
    val act3:Action = new Action(3)
    val None:Action = new Action(0)
    val actionPlus:mutable.Set[Action] = mutable.Set(None,act1,act2, act3)

    val d:Map[State,mutable.Set[Action]] = Map((s1,actionPlus),(s2,mutable.Set(None,act1,act3)),(s3,actionPlus),(s4,actionPlus),(s5,mutable.Set(None,act2,act3)),(s6,mutable.Set(None,act1)))

    val x1 = IConstant(act1.p)
    val x2 = IConstant(act2.p)
    val x3 = IConstant(act3.p)
    val g1:IFormula = (x1 >= 2 * x2) & (x3 <= 3)
    val g2:IFormula = (x1 + x2 + x3 <= 10) & (x3 > 3)
    val g3:IFormula = (x1 > 5) & (x3 > x1)
    val g4:IFormula = (x1 > 5) & (x1 + 2*x3 > 3*x2)
    val g5:IFormula = x1 === x1
    val g6:IFormula = x1 + 2 * x2 >= x3
    val g7:IFormula = x2 === x3

    val xMap:Map[Int,ConstantTerm] = Map((1,x1.c),(2,x2.c),(3,x3.c))

    val guards:Map[String,IFormula] = Map(("g1",g1),("g2",g2),("g3",g3),("g4",g4),("g5",g5),("g6",g6),("g7",g7))

    val deltaSS:Map[(State, State),IFormula] = Map(((s1,s1),INot(g1) &  INot(g2)),((s1,s2),g1),((s1,s3),g2),((s2,s3),INot(g3)),((s2,s4),g3),((s3,s1),INot(g6)),((s3,s5),g6),((s4,s2),INot(g4)),((s4,s6),g4),((s5,s2),INot(g7)),((s5,s6),g7),((s6,s6),g5))

    val p:AtomicProposition = new AtomicProposition("p" )
    val q:AtomicProposition = new AtomicProposition("q")


    val lambda:Map[State,mutable.Set[AtomicProposition]] = Map((s1,mutable.Set()),(s2,mutable.Set(p)),(s3,mutable.Set(p)),(s4,mutable.Set(p)),(s5,mutable.Set(q)),(s6,mutable.Set(q)))

    val M:HDMAS_Structure = new HDMAS_Structure(agentSet,actionPlus,stateSet,d,deltaSS,lambda,xMap)


    //example_4
    val y1: Variable = new Variable(1)
    val y2: Variable = new Variable(2)

    val p1 = new AtomicProposition("p1")

    val p2 = new AtomicProposition("p2")

    val p3 = new AtomicProposition("p3")

    val part1_1_1 = new NextFormulae(p1)
    val part1_1_2 = new StrategicFormulae(y1,y2,part1_1_1)
    val part1_1_3 = new ForallQuantifierFormulae(part1_1_2,y2)
    val part1_2_1 = new UntilFormulae(new TrueAtom,p2)
    val part1_2_2 = new StrategicFormulae(y1,y2,part1_2_1)
    val part1_2_3 = new ExistQuantifierFormulae(part1_2_2,y2)
    val part1 = new StrategicFormulae(y1,new Constant(5),new UntilFormulae(part1_1_3,part1_2_3))
    val part2_1_1 = new StrategicFormulae(y1,y2,new UntilFormulae(new TrueAtom,p3))
    val part2_1_2 = new ForallQuantifierFormulae(part2_1_1,y2)
    val part2_2_1 = new StrategicFormulae(new Constant(3),y2,new NextFormulae(p1))
    val part2_2_2 = new ForallQuantifierFormulae(part2_2_1,y2)
    val part2_2_3 = new NotFormulae(part2_2_2)
    val part2 = new ExistQuantifierFormulae(new AndFormulae(part2_1_2,part2_2_3),y1)
    val formula = new ForallQuantifierFormulae(new OrFormulae(part1,part2),y1)
    val f = new Function
    println("Example 4: ")
    println(f.PUSH(QuantifierType.FORALL,y1,null,formula))
    println("\n")


    //example_5
    println("Example 5: ")
    println(f.NF(formula))
    println("\n")


    //example_6
    val formula_6 = new ExistQuantifierFormulae(new ForallQuantifierFormulae(new StrategicFormulae(y1,y2,new NextFormulae(new OrFormulae(p,q))),y2),y1)
    val NF_formula_6 = f.NF(formula_6)
    val modelChecking = new ModelChecking
    println("Example 6: ")
    println(modelChecking.GlobalMC(M,NF_formula_6,null))
    println("\n")


    //example_7
    val formula_7 = new StrategicFormulae(new Constant(7), new Constant(4),new NextFormulae(new FEQuantifierFormulae(new StrategicFormulae(y1,y2,new GloballyFormulae(p)),y2,y1)))
    println("Example 7: ")
    println(modelChecking.GlobalMC(M,formula_7,null))
    println("\n")


    //example_8
    val formula_8 = new StrategicFormulae(new Constant(6), new Constant(3), new NextFormulae(new ExistQuantifierFormulae(new StrategicFormulae(y1,new Constant(10),new UntilFormulae(new FEQuantifierFormulae(new StrategicFormulae(y1,y2,new GloballyFormulae(p)),y2,y1),new ForallQuantifierFormulae(new StrategicFormulae(new Constant(0),y2,new GloballyFormulae(q)),y2))),y1)))
    println("Example 8: ")
    println(modelChecking.GlobalMC(M,formula_8,null))


  }



}
