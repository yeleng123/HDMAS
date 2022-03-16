package formulae

object FormulaeType extends Enumeration {
  type  FormulaeType = Value

  //Path formulae
  val NEXT:Value = Value("NEXT")
  val GLOBALLY:Value = Value("GLOBALLY")
  val UNTIL:Value = Value("Until")

  //State formulae
  val AND:Value = Value("AND")
  val OR:Value = Value("OR")
  val FORALL:Value = Value("FORALL")
  val EXISTS:Value = Value("EXISTS")
  val FF:Value = Value("FF") //forall forall
  val EE:Value = Value("EE") //exists exists
  val FE:Value = Value("FE") //forall exists
  val EF:Value = Value("EF") // exists forall
  val ATOMIC:Value = Value("ATOMIC")
  val TRUE_ATOM:Value =Value("TRUE_ATOM")
  val FALSE_ATOM:Value =Value("FALSE_ATOM")
  val NOT:Value = Value("NOT")
  val STRATEGIC:Value = Value("STRATEGIC")

  //Term
  val CONSTANT:Value = Value("CONSTANT")
  val VARIABLE:Value = Value("VARIABLE")

  val IMPLICATION:Value = Value("IMPLICATION")
  val DOUBLE_IMPL:Value = Value("DOUBLE_IMPL")

  //val ATOMIC:Value = Value("ATOMIC")
  val EQUALITY:Value = Value("EQUALITY")




}
