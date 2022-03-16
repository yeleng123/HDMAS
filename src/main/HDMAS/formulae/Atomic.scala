package formulae

import formulae.FormulaeType.{ATOMIC, FormulaeType}

import java.util
//import scala.beans.BeanProperty

class Atomic extends AtomicFormulae {
  var predicate:Predicate = _
  var arguments:util.LinkedList[Term] = new util.LinkedList()
  var nArity:Int = -1 //跟踪参数的最大数量，如果=-1则被忽略（谓词尚未设置）。
  var nArgs:Int = 0 //Keeps track of the current argument number

  def this(predicate: Predicate) = {
    this()
    this.predicate = predicate
    this.nArity = predicate.getArity
    this.arguments = new util.LinkedList()
  }

  def this(predicate: Predicate, arguments:Term*) = {//可变参数
    this(predicate)
    for (arg:Term <- arguments){
      if (nArgs < nArity){
        this.arguments.add(arg)
        nArgs = nArgs + 1
      }else{ throw new RuntimeException("Too many arguments")}
    }
  }

  def getPredicate:Predicate = this.predicate

  def setPredicate(predicate: Predicate):Unit = { //类似把P(t1,t2)转为P(t1,t2,t3)??
    if (predicate.getArity >= this.nArgs) {
      this.predicate = predicate
      nArity = predicate.getArity
    } else{
      throw new RuntimeException("Predicate arity is too low")
    }
  }

  def getArguments:util.LinkedList[Term] = this.arguments

  def addArguments(arguments:Term*):Unit = {
    for (arg:Term <- arguments){
      if (nArgs < nArity || nArity <=0) {
        this.arguments.add(arg)
        nArgs = nArgs +1
      } else {throw new RuntimeException("Too many arguments")}
    }
  }

  override def substitute(oldOne: Variable, newOne: Term): Unit = {
    val newList:util.LinkedList[Term] = new util.LinkedList[Term]()
    val i: util.Iterator[Term] = this.arguments.iterator()

    while(i.hasNext) {
      val arg:Term = i.next()
      if (arg == oldOne){
        newList.add(newOne)
      } else {newList.add(arg)}
    }

    this.arguments = newList

    //this.formulaFactory(this.getFormulaType,null, null,this.getPredicate,newList)

  }

  override def toString: String = {
    var s:String = predicate.toString + "("

    if (arguments.size() == nArity || nArity <= 0) {
      var i:Int = 0
      val j:util.Iterator[Term] = arguments.iterator()

      while(j.hasNext) {
        val arg:Term = j.next()
        if(i>0){s = s + ", "}
        s = s + arg.toString
        i = i + 1
      }
    }else{
      throw new RuntimeException("Incomplete arguments list")
    }
    s = s+")"
    s
  }

  override def getFormulaType: FormulaeType = ATOMIC

  override def hashCode(): Int = {
    var res:Int = if(this.getPredicate != null) this.getPredicate.hashCode() else 0

    val i:util.Iterator[Term] = this.getArguments.iterator()

    while(i.hasNext){
      val t:Term = i.next()
      res = 31*res
      res = res+(if(t!=null)t.hashCode() else 0)
    }
    res
  }

  override def copy(): Formulae = {
    val newArgs:util.LinkedList[Term] = new util.LinkedList()
    val i:util.Iterator[Term] = this.getArguments.iterator()

    while(i.hasNext){
      val t:Term = i.next()
      newArgs.add(t)
    }
    this.formulaFactory(this.getFormulaType,null,null,this.getPredicate.copy(),newArgs)
  }

  override def equals(obj: Any): Boolean = {
    var res:Boolean = false

    if (obj != null && this.getClass.equals(obj.getClass)){
      val other:Atomic = obj.asInstanceOf[Atomic]
      res = this.getPredicate.equals(other.getPredicate) && this.getArguments.equals(other.getArguments)
    }
    res
  }
}

