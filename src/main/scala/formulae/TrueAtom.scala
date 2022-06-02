package formulae
import formulae.FormulaeType.{FormulaeType, TRUE_ATOM}

class TrueAtom extends AtomicFormulae {
  override def toString: String = "TRUE"

  override def getFormulaType: FormulaeType = TRUE_ATOM

  override def negate(): Formulae = new FalseAtom

  override def substitute(oldOne: Variable, newOne: Term): Unit = null

  // def toPrincess = true

}
