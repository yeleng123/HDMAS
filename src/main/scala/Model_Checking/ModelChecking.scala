package Model_Checking

import Structure.{Action, Agent, HDMAS_Structure, State}
import ap.SimpleAPI
import ap.parser.IExpression._
import ap.parser._
import ap.types.Sort.Nat
import ap.types.SortedConstantTerm
import formulae.{Variable, _}



//import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.mutable

class ModelChecking extends Cloneable{

  def gQ(M:HDMAS_Structure,Q:mutable.Set[State],s:State):IFormula = {

    val delta = M.tg
    val deltaSet:mutable.Set[IFormula] = mutable.Set()
    for (i <- Q) {
      if (delta.contains((s,i))){deltaSet.add(delta(s,i))}
    }

    if (deltaSet.nonEmpty) {
      var gq:IFormula = deltaSet.head
      deltaSet.foreach(x => gq = gq | x)
      //println(gq)
      gq
    } else {IExpression.Boolean2IFormula(false)} }//{throw new RuntimeException("This state s has no connection with all states in Q")} }




  def PrF(M:HDMAS_Structure,s:State,t1:ITerm,t2:ITerm,Q:mutable.Set[State]):IFormula = {
    //t1 t2 只可能为Int或Variable
    //val p = spawnWithAssertions
    //import p._
    import IExpression._

    val actionSet:mutable.Set[Action] = M.getActionSet
    val actionInS = M.AAF(s)

    var kSet:Map[Int,ConstantTerm] = Map()
    var lSet:Map[Int,ConstantTerm] = Map()


    actionSet.foreach(x => {
      kSet += (x.getAction -> new SortedConstantTerm(s"k_${x.getAction}",Nat))
      lSet += (x.getAction -> new SortedConstantTerm(s"l_${x.getAction}",Nat))
    })

    var controllableList = List[ConstantTerm]()
    var uncontrollableList = List[ConstantTerm]()
    kSet.foreach(x => controllableList = controllableList :+ x._2)
    lSet.foreach(x => uncontrollableList = uncontrollableList :+ x._2)




    val sumK:IFormula = sum(controllableList) === t1
    val sumL:IFormula = sum(uncontrollableList) === t2


    var k = List[IFormula]()
    var numK = 0
    actionSet.foreach(x => {
      if (controllableList(numK).name != "k_0" ){ //0表示epsilon
        if (actionInS.contains(x)){
          k = k :+ ((controllableList(numK) =/= 0) ==> true)
        } else {k = k :+ ((controllableList(numK) =/= 0) ==> false)}
        numK = numK + 1
      } else { numK = numK + 1}
    })
    val andK = and(k)

    var l = List[IFormula]()
    var numL = 0
    actionSet.foreach(x => {
      if (uncontrollableList(numL).name != "l_0"){
        if (actionInS.contains(x)){
          l = l :+ ((uncontrollableList(numL) =/= 0) ==> true)
        } else {l = l :+ ((uncontrollableList(numL) =/= 0) ==> false)}
        numL = numL + 1
      } else {numL = numL + 1}
    })
    val andL = and(l)

    val actionSize = M.getActionSet.size-1

    var formula:IFormula = gQ(M, Q, s)
    //var sub:Map[ConstantTerm,ITerm] = Map[ConstantTerm,ITerm]()
    if (formula != IExpression.Boolean2IFormula(false)) {
      for (i <- 1 to actionSize){
        // sub += (new ConstantTerm(s"x_$i") -> (new ConstantTerm(s"k_$i") + new ConstantTerm(s"l_$i")))
        formula = ConstantSubstVisitor(formula, Map(M.getActionCounter(i) -> (kSet(i) + lSet(i))))
      }
    }

    //val formula = ConstantSubstVisitor(gQ,sub)
    val part = (andL & sumL) ==> formula
    // println("formula"+formula)
    val forallL = quanConsts(Quantifier.ALL,uncontrollableList,part)
    quanConsts(Quantifier.EX,controllableList,andK & sumK & forallL)
  }


  //def PREIMG(M:HDMAS_Structure,t1:ITerm,t2:ITerm,Q:mutable.Set[State],assignment:IFunction,pre:String,y1:IVariable,y2:IVariable): mutable.Set[State] ={
  def PREIMG(M:HDMAS_Structure,t1:ITerm,t2:ITerm,Q:mutable.Set[State],assignment:List[Int],pre:String,y1:IConstant,y2:IConstant): mutable.Set[State] ={
    //y1 y2 means real variable y1 y2 , not position

    //withProver { p =>
    val p = SimpleAPI.spawn
      import IExpression._
      import p._
      var newt1 = t1


      if (t1.getClass == IConstant && y1 != null){
        val t1New = t1.asInstanceOf[IConstant]
        if (y1.c.name != t1New.c.name ) {
          newt1 = Int2ITerm(assignment.head)
        }
      }


      var newt2 = t2
      if (t2.getClass == IConstant && y2 != null) {
        val t2New = t2.asInstanceOf[IConstant]
        if (y2.c.name != t2New.c.name ){
          newt2 = Int2ITerm(assignment(1))
        }
      }


      var Z = mutable.Set[State]()
      val stateSet = M.getStateSet.clone()
      //println(stateSet)
      stateSet.foreach(x => {
        scope{
          pre match {
            case "eps" =>
              !! (PrF(M,x,newt1,newt2,Q))
              //println(PrF(M,x,newt1,newt2,Q))
              if ( ??? == SimpleAPI.ProverStatus.Sat ){ Z += x}

            case "Exists" =>
              val Newt1 = newt1.asInstanceOf[IConstant]
              val formula = quanConsts(Quantifier.EX,List(Newt1.c),PrF(M,x,Newt1,newt2,Q))
              !! (formula)
              if (??? == SimpleAPI.ProverStatus.Sat){Z += x}

            case "Forall" =>
              val Newt2 = newt2.asInstanceOf[IConstant]
              val formula = quanConsts(Quantifier.ALL,List(Newt2.c),PrF(M,x,newt1,Newt2,Q))
              !! (formula)
              if (??? == SimpleAPI.ProverStatus.Sat){Z += x}

            case "EF" =>
              val Newt1 = newt1.asInstanceOf[IConstant]
              val Newt2 = newt2.asInstanceOf[IConstant]
              val formula1 = quanConsts(Quantifier.ALL,List(Newt2.c),PrF(M,x,Newt1,Newt2,Q))
              val formula2 = quanConsts(Quantifier.EX,List(Newt1.c),formula1)
              !! (formula2)
              if (??? == SimpleAPI.ProverStatus.Sat){Z += x}

            case "FE" =>
              val Newt1 = newt1.asInstanceOf[IConstant]
              val Newt2 = newt2.asInstanceOf[IConstant]
              val formula1 = quanConsts(Quantifier.EX,List(Newt1.c),PrF(M,x,Newt1,Newt2,Q))
              val formula2 = quanConsts(Quantifier.ALL,List(Newt2.c),formula1)
              !! (formula2)
              if (??? == SimpleAPI.ProverStatus.Sat){Z += x}
              //println(Z)

            case _ => throw new RuntimeException("Incorrect quantifier type")


          }
        }


      })
    p.shutDown
    Z


  }


  def G_Fixpoint(M:HDMAS_Structure,t1:ITerm,t2:ITerm,phi:Formulae,assignment:List[Int],pfix:String,y1:IConstant,y2:IConstant):mutable.Set[State] = {
    val Q = GlobalMC(M,phi,assignment)
    var W = M.getStateSet.clone()
    var Z = Q.clone()

    while ((Z & W) != W){
      W = Z
      Z = PREIMG(M,t1,t2,W,assignment,pfix,y1,y2) & Q
    }
    Z
  }

  def U_FixPoint(M:HDMAS_Structure,t1:ITerm,t2:ITerm,phi1:Formulae,phi2:Formulae,assignment:List[Int],pfix:String,y1:IConstant,y2:IConstant):mutable.Set[State] = {
    val Q1 = GlobalMC(M,phi1,assignment)
    val Q2 = GlobalMC(M,phi2,assignment)
    var W = mutable.Set[State]()
    var Z = Q2.clone()

    while((Z & W) != Z) {
      W = Z
      Z = Q2 ++ (PREIMG(M,t1,t2,W,assignment,pfix,y1,y2) & Q1)
    }

    Z
  }

  def GlobalMC(M:HDMAS_Structure,phi:Formulae,assignment: List[Int]):mutable.Set[State] = phi.getFormulaType match {
    case FormulaeType.ATOMIC =>
      val set = mutable.Set[State]()
      for (i <- M.getStateSet.clone()) {
        if (M.LF(i).contains(phi.asInstanceOf[AtomicProposition])) {set += i}
      }
      set


    case FormulaeType.NOT =>
      val formula = phi.asInstanceOf[NotFormulae]
      val nested = formula.getNestedFormula
      val set = GlobalMC(M,nested,assignment)
      val notSet = M.getStateSet.clone()
      set.foreach(x => notSet -= x)
      notSet


    case FormulaeType.AND =>
      val formula = phi.asInstanceOf[AndFormulae]

      val left = formula.getLeftFormula
      val right = formula.getRightFormula
      val set1 = GlobalMC(M,left,assignment)
      val set2 = GlobalMC(M,right,assignment)
      set1 & set2


    case FormulaeType.OR =>
      val formula = phi.asInstanceOf[OrFormulae]
      val left = formula.getLeftFormula
      val right = formula.getRightFormula
      val set1 = GlobalMC(M,left,assignment)
      val set2 = GlobalMC(M,right,assignment)
      set1 ++ set2


    case FormulaeType.STRATEGIC =>
      val formula = phi.asInstanceOf[StrategicFormulae]
      val pfix = "eps"
      // 检查，更改t1 t2 类型从any to Term
      val t1 = formula.getControllableAgent.toPrincess
      val t2 = formula.getUncontrollableAgent.toPrincess
      val nested1 = formula.getNestedFormula
      nested1.getFormulaType match {
        case FormulaeType.NEXT =>
          val nested2 = nested1.asInstanceOf[NextFormulae]
          val nested3 = nested2.getRightFormula
          val Q = GlobalMC(M,nested3,assignment)
          PREIMG(M,t1,t2,Q,assignment,pfix,null,null)

        case FormulaeType.GLOBALLY =>
          val nested2 = nested1.asInstanceOf[GloballyFormulae]
          val nested3 = nested2.getRightFormula
          G_Fixpoint(M,t1,t2,nested3,assignment,pfix,null,null)

        case FormulaeType.UNTIL =>
          val nested2 = nested1.asInstanceOf[UntilFormulae]
          val left = nested2.getLeftFormula
          val right = nested2.getRightFormula
          U_FixPoint(M,t1,t2,left,right,assignment,pfix,null,null)
      }


    case FormulaeType.EXISTS => // E y1
      val formula = phi.asInstanceOf[ExistQuantifierFormulae]
      val pfix = "Exists"
      val y1 = formula.getQuantifiedVariable1.toPrincess
      val nested1 = formula.getNestedFormula
      if (nested1.getFormulaType == FormulaeType.STRATEGIC) {
        val nested2 = nested1.asInstanceOf[StrategicFormulae]
        val t1 = nested2.getControllableAgent.toPrincess
        val t2 = nested2.getUncontrollableAgent.toPrincess
        val nested3 = nested2.getNestedFormula //path formula
        val nested4 = nested3.getRightFormula //phi
        nested3.getFormulaType match {
          case FormulaeType.NEXT =>
            val Q = GlobalMC(M,nested4,assignment)
            PREIMG(M,t1,t2,Q,assignment,pfix,y1,null)
          case FormulaeType.GLOBALLY =>
            G_Fixpoint(M,t1,t2,nested4,assignment,pfix,y1,null)
          case FormulaeType.UNTIL =>
            val left = nested3.getLeftFormula
            U_FixPoint(M,t1,t2,left,nested4,assignment,pfix,y1,null)
        }
      } else {throw new RuntimeException("Incorrect NF Formula")}


    case FormulaeType.FORALL => // A y2
      val formula = phi.asInstanceOf[ForallQuantifierFormulae]
      val pfix = "Forall"
      val y2 = formula.getQuantifiedVariable1.toPrincess
      val nested1 = formula.getNestedFormula
      if (nested1.getFormulaType == FormulaeType.STRATEGIC){
        val nested2 = nested1.asInstanceOf[StrategicFormulae]
        val t1 = nested2.getControllableAgent.toPrincess
        val t2 = nested2.getUncontrollableAgent.toPrincess
        val nested3 = nested2.getNestedFormula //path formula
        val nested4 = nested3.getRightFormula //phi
        nested3.getFormulaType match {
          case FormulaeType.NEXT =>
            val Q = GlobalMC(M,nested4,assignment)
            PREIMG(M,t1,t2,Q,assignment,pfix,null,y2)
          case FormulaeType.GLOBALLY =>
            G_Fixpoint(M,t1,t2,nested4,assignment,pfix,null,y2)
          case FormulaeType.UNTIL =>
            val left = nested3.getLeftFormula
            U_FixPoint(M,t1,t2,left,nested4,assignment,pfix,null,y2)
        }
      } else {throw new RuntimeException("Incorrect NF Formula")}


    case FormulaeType.EF =>
      val formula = phi.asInstanceOf[EFQuantifierFormulae]
      val pfix = "EF"
      val y1 = formula.getQuantifiedVariable1.toPrincess
      val y2 = formula.getQuantifiedVariable2.toPrincess
      val nested1 = formula.getNestedFormula
      if (nested1.getFormulaType == FormulaeType.STRATEGIC){
        val nested2 = nested1.asInstanceOf[StrategicFormulae]
        val t1 = nested2.getControllableAgent.toPrincess
        val t2 = nested2.getUncontrollableAgent.toPrincess
        val nested3 = nested2.getNestedFormula //path formula
        val nested4 = nested3.getRightFormula //phi
        nested3.getFormulaType match {
          case FormulaeType.NEXT =>
            val Q = GlobalMC(M,nested4,assignment)
            PREIMG(M,t1,t2,Q,assignment,pfix,y1,y2)
          case FormulaeType.GLOBALLY =>
            G_Fixpoint(M,t1,t2,nested4,assignment,pfix,y1,y2)
          case FormulaeType.UNTIL =>
            val left = nested3.getLeftFormula
            U_FixPoint(M,t1,t2,left,nested4,assignment,pfix,y1,y2)
        }
      } else {throw new RuntimeException("Incorrect NF Formula")}

    case FormulaeType.FE =>
      val formula = phi.asInstanceOf[FEQuantifierFormulae]
      val pfix = "FE"
      val y1 = formula.getQuantifiedVariable2.toPrincess
      val y2 = formula.getQuantifiedVariable1.toPrincess
      val nested1 = formula.getNestedFormula
      if (nested1.getFormulaType == FormulaeType.STRATEGIC){
        val nested2 = nested1.asInstanceOf[StrategicFormulae]
        val t1 = nested2.getControllableAgent.toPrincess
        val t2 = nested2.getUncontrollableAgent.toPrincess
        val nested3 = nested2.getNestedFormula //path formula
        val nested4 = nested3.getRightFormula //phi
        nested3.getFormulaType match {
          case FormulaeType.NEXT =>
            val Q = GlobalMC(M,nested4,assignment)
            PREIMG(M,t1,t2,Q,assignment,pfix,y1,y2)
          case FormulaeType.GLOBALLY =>
            G_Fixpoint(M,t1,t2,nested4,assignment,pfix,y1,y2)
          case FormulaeType.UNTIL =>
            val left = nested3.getLeftFormula
            U_FixPoint(M,t1,t2,left,nested4,assignment,pfix,y1,y2)
        }
      } else {throw new RuntimeException("Incorrect NF Formula")}

    case _ => throw new RuntimeException("Incorrect NF Formula Type")


  }





}

object ModelChecking{
  def main(args: Array[String]): Unit ={
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

    val agNum:Int = 11

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
    val g1:IFormula = (x1 >= 2 * x2) & (x3 <= 3) //& (x1>=0) & (x2>=0) & (x3>=0)
    val g2:IFormula = (x1 + x2 + x3 <= 10) & (x3 > 3) //& (x1>=0) & (x2>=0) & (x3>=0)
    val g3:IFormula = (x1 > 5) & (x3 > x1)//& (x1>=0) & (x3>=0)
    val g4:IFormula = (x1 > 5) & (x1 + 2*x3 > 3*x2) //& (x1>=0) & (x2>=0) & (x3>=0)
    val g5:IFormula = (x1 === x1 )// & (x1>=0)
    val g6:IFormula = (x1 + 2 * x2 >= x3)// & (x1>=0) & (x2>=0) & (x3>=0)
    val g7:IFormula = (x2 === x3)// & (x2>=0) & (x3>=0)

    val xMap:Map[Int,ConstantTerm] = Map((1,x1.c),(2,x2.c),(3,x3.c))

    val guards:Map[String,IFormula] = Map(("g1",g1),("g2",g2),("g3",g3),("g4",g4),("g5",g5),("g6",g6),("g7",g7))

    val deltaSS:Map[(State, State),IFormula] = Map(((s1,s1),INot(g1) &  INot(g2)),((s1,s2),g1),((s1,s3),g2),((s2,s3),INot(g3)),((s2,s4),g3),((s3,s1),INot(g6)),((s3,s5),g6),((s4,s2),INot(g4)),((s4,s6),g4),((s5,s2),INot(g7)),((s5,s6),g7),((s6,s6),g5))

    val p:AtomicProposition = new AtomicProposition("p" )
    val q:AtomicProposition = new AtomicProposition("q")
    //println(p == q)

    val lambda:Map[State,mutable.Set[AtomicProposition]] = Map((s1,mutable.Set()),(s2,mutable.Set(p)),(s3,mutable.Set(p)),(s4,mutable.Set(p)),(s5,mutable.Set(q)),(s6,mutable.Set(q)))

    val M:HDMAS_Structure = new HDMAS_Structure(actionPlus,stateSet,d,deltaSS,lambda,xMap)

    val test = new ModelChecking
    val y1 = new Variable(1)
    val y2 = new Variable(2)
    val phi:Formulae = new StrategicFormulae(new Constant(7), new Constant(4),new NextFormulae(new FEQuantifierFormulae(new StrategicFormulae(y1,y2,new GloballyFormulae(p)),y2,y1)))
    val phi2:Formulae = new StrategicFormulae(new Constant(6), new Constant(3), new NextFormulae(new ExistQuantifierFormulae(new StrategicFormulae(y1,new Constant(10),new UntilFormulae(new FEQuantifierFormulae(new StrategicFormulae(y1,y2,new GloballyFormulae(p)),y2,y1),new ForallQuantifierFormulae(new StrategicFormulae(new Constant(0),y2,new GloballyFormulae(q)),y2))),y1)))

    println(test.GlobalMC(M,phi,null))
    println(test.GlobalMC(M,phi2,null))


    /*
    //val x1  = new ConstantTerm("k_1") + new ConstantTerm("l_1")
    //val x2  = new ConstantTerm("k_2") + new ConstantTerm("l_2")
    //val x3  = new ConstantTerm("k_3") + new ConstantTerm("l_3")

    //val x2:IVariable = act2.p
    //val x3:IVariable = act3.p
    //val x1 = IVariable(101,Nat) + IVariable(201,Nat)
    //val x2 = IVariable(102,Nat) + IVariable(202,Nat)
    //val x3 = IVariable(103,Nat) + IVariable(203,Nat)
    val g1:IFormula = (x1 >= 2 * x2) & (x3 > 3)
    val g2:IFormula = (x1 + x2 + x3 <= 10) & (x3 > 3)
    val g3:IFormula = (x1 > 5) & (x3 > x1)
    val g4:IFormula = (x1 > 5) & (3 * x2 < x1 + 2 * x3)
    val g5:IFormula = x1 === x1
    val g6:IFormula = x1 + 2 * x2 >= x3
    val g7:IFormula = x2 === x3

    val guards:Map[String,IFormula] = Map(("g1",g1),("g2",g2),("g3",g3),("g4",g4),("g5",g5),("g6",g6),("g7",g7))

    val deltaSS:Map[(State, State),IFormula] = Map(((s1,s1),INot(g1) &  INot(g2)),((s1,s2),g1),((s1,s3),g2),((s2,s3),INot(g3)),((s2,s4),g3),((s4,s3),INot(g4)),((s4,s6),g4),((s5,s2),INot(g7)),((s5,s6),g7),((s6,s6),g5))


    val p:AtomicProposition = new AtomicProposition("p" )
    val q:AtomicProposition = new AtomicProposition("q")
    //println(p == q)

    val lambda:Map[State,mutable.Set[AtomicProposition]] = Map((s1,mutable.Set()),(s2,mutable.Set(p)),(s3,mutable.Set(p)),(s4,mutable.Set(p)),(s5,mutable.Set(q)),(s6,mutable.Set(q)))

    val M:HDMAS_Structure = new HDMAS_Structure(agentSet,actionPlus,stateSet,d,deltaSS,lambda)

    val y1:Variable = new Variable("1")
    val y2:Variable = new Variable("2")

    //println(y1 == y2)

    val phi:Formulae = new StrategicFormulae(new Constant("7"), new Constant("4"),new NextFormulae(new FEQuantifierFormulae(new StrategicFormulae(y1,y2,new GloballyFormulae(p)),y2,y1)))
    //val phi = new FEQuantifierFormulae(new StrategicFormulae(y1,y2,new GloballyFormulae(p)),y2,y1)

    val fortress = new Model_Checking.ModelChecking

    //println(fortress.GlobalMC(M,phi,null))
    val set1 = fortress.GlobalMC(M,phi,null)
    println(set1)
    //val set1 = fortress.G_Fixpoint(M,y1.toPrincess,y2.toPrincess,phi,null,"FE",y1.toPrincess,y2.toPrincess)
    //println(fortress.PREIMG(M,y1.toPrincess,y2.toPrincess,set1,null,"FE",y1.toPrincess,y2.toPrincess))
    //println(fortress.PREIMG(M,y1.toPrincess,y2.toPrincess,set1,null,"FE",y1.toPrincess,y2.toPrincess))

     */
  }
}