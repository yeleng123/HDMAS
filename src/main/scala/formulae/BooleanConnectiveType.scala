package formulae

object BooleanConnectiveType extends Enumeration {

  type BooleanConnectiveType = Value

  val NOT:Value = Value("NOT")
  val AND:Value = Value("AND")
  val OR:Value = Value("OR")
  val IMPL:Value = Value("IMPL")
  val DOUBLE_IMP:Value = Value("DOUBLE_IMPL")


  //??? 恒真恒假可以为boolean connectives嘛？
  val TRUE:Value = Value("TRUE") //恒真
  val FALSE:Value =Value("FALSE")  //恒假

}
