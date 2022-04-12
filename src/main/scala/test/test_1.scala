package test

import Structure.State
import ap.SimpleAPI._
import ap.SimpleAPI._
import ap.terfor._
//import ap.parser.IExpression.ConstantTerm
import ap.types.Sort.Nat

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
//import ap.SimpleAPI.withProver
import ap.parser._

import scala.collection.mutable
import Structure.{Action, Agent, HDMAS_Structure, State}
import ap.SimpleAPI
//import ap.parser.Environment.Variable
import ap.parser.IExpression._
import ap.parser._
import ap.SimpleAPI._
import ap.interpolants.StructuredPrograms.{Assignment, assignedVars}
import ap.types.Sort.Nat
import formulae.{Variable, _}

object test_1 {
  def main(args: Array[String]):Unit = {
    /*
    val p = spawnWithAssertions
    import p._
    import IExpression._

    val l = createConstants(5, 0 to 10)
    println(l(0))
    val j = createExistentialConstants(5)
    println(j)
    // p !! j(0) > 6
    var list = List[ConstantTerm]()
    for (i <- 1 to 10) {list = list :+ new ConstantTerm(s"Term$i")}
    //p !! quanConsts(Quantifier.EX,list,list(2) >0)
    println(list.getClass)
    // p !! ex(list(0) => list(0) >0)
    val b = 1 to 10
    println(b.getClass)
    println(list)
    val a = v(10)
    println(a)
    //p !! ex(a => a>60 )
    //println(???)

    /* p !! ex( x => x > 5)
    println(???)
    println() */
    val s = Set("a","B","C")
    println(s)

    val x= IVariable(1,Nat)
    var y = IConstant(new ConstantTerm("1"))
    p !! quanVars(Quantifier.ALL,List(x),x > 5)
    p !! IExpression.Int2ITerm(4) > x
    println(???)



    def AD(n:Int, m:Int,p:Int, l:ListBuffer[Int], idx:Int):Unit = {
      if (m == 1) {
        l.update(idx,n)
        //P(state) += l
        println(l)
      }
      if (idx < p-1){
        for (i <- 0 to n){
          l.update(idx,i)
          AD(n-i,m-1,p,l,idx+1)
        }}
      }

    val n = 10
    val m = 3
    val ll = ListBuffer.fill(m)(0)
    //ll.update(0,6)
    println(ll)
    AD(n,m,m,ll,0)
    def mm(a:Int):Unit = {
      println(a)
      if (a > 0){mm(a-1)}
    }

    mm(4)

    for (i <- 0 to 0){println(i)}

    val bb = Set(1,2,3)
    val aa = Set(1,2)

    if ((bb & aa) !=aa) {aa + 4}


    val y = IVariable(1,Nat)
    withProver { p =>
      import p._
      import IExpression._
      !! (quanVars(Quantifier.ALL,List(y),y>5))
      println(???)
    }

    withProver { p =>
      import p._
      import IExpression._
      !! (quanVars(Quantifier.EX,List(y),y>5))
      println(???)
    }

    val f1:ITerm = IVariable(1,Nat) + IVariable(2)
    val f2:ITerm = new ConstantTerm("p1") + new ConstantTerm("p2")
    val f11:IVariable = IVariable(1)
    withProver { p =>
      import p._
      import IExpression._
      val form = (quanVars(Quantifier.EX,List(f11),f1>0 & f1<10))
      !! (form)
      println(form)
      println(???)
    }

    val x1:ITerm = new ConstantTerm("k_1") + new ConstantTerm("l_1")
    val x2:ConstantTerm = new ConstantTerm("k_1")
    withProver { p =>
      import p._
      import IExpression._
      val form = quanConsts(Quantifier.EX,List(x2),x2>0 & x1<10)
      println(form)
      !! (form)

      println(???)
    }
    */
    // create variable from y1 to y10 and put them in the list
    var list = List[ConstantTerm]()
    for (i <- 1 to 10){
      list = list :+ new ConstantTerm(s"$i")
    }
    println(list)
    println("**********************")

    // y1+...+y5=10
    val formula1 = list.head + list(1) + list(2) + list(3) + list(4) === 10
    //y5+...+y10=10
    val formula2 = list(5) + list(6) + list(7) + list(8) + list(9) === 10
    //A y6 A y7 A y8 A y9 A y10 (y1+y2+y3+y4+y5 ===10 & y6+y7+y8+y9+y10 ===10 )
    val quan1 = quanConsts(Quantifier.EX,list.slice(0,5),formula1 & formula2)
    println(quan1)
    println("**********************")
    //E y1 E y2 E y3 E y4 E y5(A y6 A y7 A y8 A y9 A y10 (y1+y2+y3+y4+y5 ===10 & y6+y7+y8+y9+y10 ===10 ) )
    val quan2 = quanConsts(Quantifier.ALL,list.slice(5,10),quan1)
    println(quan2)


    withProver { p =>
      import p._
      import IExpression._
      scope{
        !! (quan2)
        println(???)
      }

      scope{
        !! (quanConsts(Quantifier.EX,list.slice(0,3),list(1)>10))
        println(???)
      }



    }



     withProver { p =>
      import p._
      import IExpression._

      /*
      val list = p.createConstantsRaw("x", 0 until 10)
      println("list:", list)
      val formula1 = list(0) > list(7) | list(2) > 4
      val formula2 = ConstantSubstVisitor(formula1, Map(new ConstantTerm("0") -> (list(1) + list(2))))
      println(formula2)


      val quan1 = quanConsts(Quantifier.EX, list.slice(5, 10), formula1)
      println(quan1)
      val quan2 = quanConsts(Quantifier.ALL, list.slice(0, 5), quan1)

      var x = 0
      println(quan2)

      scope {
        ?? (quan2)
        println(???)
        x = x + 10
      }
      println(x)

       */
      val y1 = IConstant(new ConstantTerm("y1"))
      val y2 = IConstant(new ConstantTerm("y2"))
      val list1 = List(new ConstantTerm("k_0"),new ConstantTerm("k_1"),new ConstantTerm("k_2"),new ConstantTerm("k_3"))
      val list2 = List(new ConstantTerm("l_0"),new ConstantTerm("l_1"),new ConstantTerm("l_2"),new ConstantTerm("l_3"))
      val formula1 = ((list1(1)=/= 0) ==> true) & ((list1(2)=/= 0) ==> false) & ((list1(3)=/= 0) ==> false)
       val formula2 = ((list2(1)=/= 0) ==> false) & ((list2(2)=/= 0) ==> false) & ((list2(3)=/= 0) ==> false)
       val list1Sum = sum(list1)=== y1
       val list2Sum = sum(list2) === y2
       val finalFormula1 = quanConsts(Quantifier.ALL,list2,(formula2&list2Sum)==>false)
       //val finalFormula2 = quanConsts(Quantifier.ALL,List(y2.c),finalFormula1)
       val finalFormula2 = quanConsts(Quantifier.EX,list1,formula1 & list1Sum & finalFormula1)
       val finalFormula3 = quanConsts(Quantifier.ALL,List(y2.c),finalFormula2)
       val finalFormula4 = quanConsts(Quantifier.EX,List(y1.c),finalFormula3)
       //val formula11 = quanConsts(Quantifier.EX,list1,formula1&list1Sum)
      //!! (quanConsts(Quantifier.ALL,List(y2.c),finalFormula3))
       //println(finalFormula4)
       //!! (quanConsts(Quantifier.ALL,List(y2.c),finalFormula2))

       //!! (formula1)
       //println("formula1")
       //!! (finalFormula4)
       val t1 = quanConsts(Quantifier.EX,List(y2.c),y1.c+y2.c === -10 )
       val t2 = quanConsts(Quantifier.ALL,List(y1.c),t1)
       val t3 = quanConsts(Quantifier.EX,list1,sum(list1)+sum(list2)===10)
       val t4 = quanConsts(Quantifier.ALL,list2,formula2 & t3)
       val ct = new ConstantTerm("y1")
       val ct1 = IConstant(ct)
       val ct2 = IConstant(ct)
       println(ct1==ct2)
       !! (t2)
       println(t3)
       println(t4)
       println("test result")
      println(???)
    }

    for (i <- 1 to 10) {
      println(i)
    }



    val t1 = IConstant(new ConstantTerm("7"))
    val t2 = Int2ITerm(7)
    val t3 = 7
    println(t1)
    println(t2==t3)




  }


}
