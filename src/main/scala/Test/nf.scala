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
object nf {
  def main(args: Array[String]): Unit = {
    val y1: Variable = new Variable(1)
    val y2: Variable = new Variable(2)

    val p1 = new AtomicProposition("p1")
    val p2 = new AtomicProposition("p2")
    val p = new AtomicProposition("p")

    val nf = new Function

    val f1:Formulae = new UntilFormulae(p1,p2)
    val f2:Formulae = new StrategicFormulae(new Constant(3),new Constant(5),new GloballyFormulae(p))
    val f3:Formulae = new ForallQuantifierFormulae(new StrategicFormulae(y1,new Constant(6),new NextFormulae(p)),y1)
    val f4:Formulae = new ForallQuantifierFormulae(new ExistQuantifierFormulae(
      new StrategicFormulae(y1,y2,new GloballyFormulae(p)),y2),y1)
    val f5:Formulae = new ForallQuantifierFormulae(new AndFormulae(new NextFormulae(p1),
      new StrategicFormulae(new Constant(5),y2,new GloballyFormulae(p2))),y2)
    val f6:Formulae = new ExistQuantifierFormulae(new AndFormulae(new GloballyFormulae(p1),
      new OrFormulae(p2,new StrategicFormulae(y1,new Constant(10),new NextFormulae(p2)))),y1)
    val f7:Formulae = new ForallQuantifierFormulae(new StrategicFormulae(new Constant(10),y2,
      new UntilFormulae(new ExistQuantifierFormulae(new StrategicFormulae(y1,y2,new NextFormulae(p)),y1),p2)),y2)
    val f8:Formulae = new ExistQuantifierFormulae(new ForallQuantifierFormulae(new AndFormulae(new UntilFormulae(p1,p2),
      new StrategicFormulae(y1,y2,new NextFormulae(p1))),y2),y1)


    // Answer
    val f1_nf:Formulae = new UntilFormulae(p1,p2)
    val f2_nf:Formulae =new StrategicFormulae(new Constant(3),new Constant(5),new GloballyFormulae(p))
    val f3_nf:Formulae = new StrategicFormulae(new Constant(0),new Constant(6),new NextFormulae(p))
    val f4_nf:Formulae = new StrategicFormulae(new Constant(0),new Constant(0),new GloballyFormulae(p))
    val f5_nf:Formulae = new AndFormulae(new NextFormulae(p1),new ForallQuantifierFormulae(
      new StrategicFormulae(new Constant(5),y2,new GloballyFormulae(p2)),y2))
    val f6_nf:Formulae = new AndFormulae(new GloballyFormulae(p1),new OrFormulae(p2,new ExistQuantifierFormulae(
      new StrategicFormulae(y1,new Constant(10),new NextFormulae(p2)),y1)))
    val f7_nf:Formulae = new ForallQuantifierFormulae(new StrategicFormulae(new Constant(10),y2,
      new UntilFormulae(new FEQuantifierFormulae(new StrategicFormulae(y1,y2,new NextFormulae(p)),y2,y1),p2)),y2)
    val f8_nf:Formulae = new AndFormulae(new UntilFormulae(p1,p2),new EFQuantifierFormulae(new StrategicFormulae(y1,y2,new NextFormulae(p1)),y1,y2))


    println("The result of NF(f1): "+nf.NF(f1))
    if(nf.NF(f1) == f1_nf){
      println("Are the result of NF(f1) and the normal form of f1 the same?  Yes.")
    } else {println("Are the result of NF(f1) and the normal form of f1 the same?  No.")}
    println("\n")


    println("The result of NF(f2): "+nf.NF(f2))
    if(nf.NF(f2) == f2_nf){
      println("Are the result of NF(f2) and the normal form of f2 the same?  Yes.")
    } else {println("Are the result of NF(f2) and the normal form of f2 the same?  No.")}
    println("\n")

    println("The result of NF(f3): "+nf.NF(f3))
    if(nf.NF(f3) == f3_nf){
      println("Are the result of NF(f3) and the normal form of f3 the same?  Yes.")
    } else {println("Are the result of NF(f3) and the normal form of f3 the same?  No.")}
    println("\n")

    println("The result of NF(f4): "+nf.NF(f4))
    if(nf.NF(f4) == f4_nf){
      println("Are the result of NF(f4) and the normal form of f4 the same?  Yes.")
    } else {println("Are the result of NF(f4) and the normal form of f4 the same?  No.")}
    println("\n")

    println("The result of NF(f5): "+nf.NF(f5))
    if(nf.NF(f5) == f5_nf){
      println("Are the result of NF(f5) and the normal form of f5 the same?  Yes.")
    } else {println("Are the result of NF(f5) and the normal form of f5 the same?  No.")}
    println("\n")

    println("The result of NF(f6): "+nf.NF(f6))
    if(nf.NF(f6) == f6_nf){
      println("Are the result of NF(f6) and the normal form of f6 the same?  Yes.")
    } else {println("Are the result of NF(f6) and the normal form of f6 the same?  No.")}
    println("\n")

    println("The result of NF(f7): "+nf.NF(f7))
    if(nf.NF(f7) == f7_nf){
      println("Are the result of NF(f7) and the normal form of f7 the same?  Yes.")
    } else {println("Are the result of NF(f7) and the normal form of f7 the same?  No.")}
    println("\n")

    println("The result of NF(f8): "+nf.NF(f8))
    if(nf.NF(f8) == f8_nf){
      println("Are the result of NF(f8) and the normal form of f1 the same?  Yes.")
    } else {println("Are the result of NF(f8) and the normal form of f1 the same?  No.")}
    println("\n")



  }


  }
