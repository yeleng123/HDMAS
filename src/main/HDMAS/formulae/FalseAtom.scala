package formulae
import formulae.FormulaeType.{FALSE_ATOM, FormulaeType}

class FalseAtom extends AtomicFormulae {
  override def toString: String = "FALSE"

  override def getFormulaType: FormulaeType = FALSE_ATOM

  override def negate():Formulae = new TrueAtom

  override def substitute(oldOne: Variable, newOne: Term): Unit = null
}
