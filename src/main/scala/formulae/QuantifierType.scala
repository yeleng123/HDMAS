package formulae

object QuantifierType extends Enumeration {

  type QuantifierType = Value

  val FORALL:Value = Value("FORALL")
  val EXISTS:Value = Value("EXISTS")
  val FF:Value = Value("FF")
  val EE:Value = Value("EE")
  val EF:Value = Value("EF")
  val FE:Value = Value("FE")

}
