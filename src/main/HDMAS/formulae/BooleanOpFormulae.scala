package formulae

import formulae.BooleanConnectiveType.BooleanConnectiveType

trait BooleanOpFormulae extends Formulae {

  def getOpType: BooleanConnectiveType

  def boolFormulaFactory(operator_type:BooleanConnectiveType, left:Formulae, right:Formulae):Formulae = operator_type match {

    case BooleanConnectiveType.NOT => new NotFormulae(left)

    case BooleanConnectiveType.AND => new AndFormulae(left,right)

    case BooleanConnectiveType.OR => new OrFormulae(left,right)

    case BooleanConnectiveType.IMPL => new ImpFormulae(left, right)

    case BooleanConnectiveType.DOUBLE_IMP => new DoubleImplFormulae(left,right)

    case _ => throw new RuntimeException("Unknown operator type")
  }

}
