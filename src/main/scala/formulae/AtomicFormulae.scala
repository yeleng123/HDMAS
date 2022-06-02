package formulae
import formulae.FormulaeType.{ATOMIC, EQUALITY, FALSE_ATOM, FormulaeType, TRUE_ATOM}

import java.util

abstract class AtomicFormulae extends  Formulae {



  override def nnf(): Formulae = {

    this.copy()
    //this.Formulae.clone()
  }

  override def negate(): Formulae = new NotFormulae(this.copy())

  override def equals(obj: Any): Boolean = {
    obj != null && this.getClass.equals(obj.getClass) //???
  }

  override def hashCode(): Int = this.getClass.hashCode()

  def formulaFactory(formulaType:FormulaeType, left:Term,right:Term,predicate: Predicate, arguments:util.LinkedList[Term]):Formulae =formulaType match {
    case TRUE_ATOM => new TrueAtom()
    case FALSE_ATOM => new FalseAtom()
    case EQUALITY => new EqualityFormulae(left,right)
    case ATOMIC =>
      val res:Formulae = new Atomic(predicate)
      val i:util.Iterator[Term] = arguments.iterator()
      while (i.hasNext) {
        val t:Term = i.next()
        res.asInstanceOf[Atomic].addArguments(t)
      }
      res
    case _ => throw new RuntimeException("Unknown formula type")
  }



}
