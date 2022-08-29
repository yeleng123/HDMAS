package Normal_Form

import formulae.{FormulaeType, _}

class Function {

  def PQE(f:Formulae): Formulae = {
    val ftype: FormulaeType.FormulaeType = f.getFormulaType
    ftype match {
      case FormulaeType.TRUE_ATOM => f

      case FormulaeType.ATOMIC => f

      case FormulaeType.NOT =>
        val nested: Formulae = f.asInstanceOf[NotFormulae].getNestedFormula
        //val nestedType: FormulaeType.FormulaeType = nested.getFormulaType
        new NotFormulae(PQE(nested))

      case FormulaeType.AND =>
        val left: Formulae = f.asInstanceOf[AndFormulae].getLeftFormula
        //val leftType: FormulaeType.FormulaeType = left.getFormulaType
        val right: Formulae = f.asInstanceOf[AndFormulae].getRightFormula
        //val rightType: FormulaeType.FormulaeType = right.getFormulaType
        new AndFormulae(PQE(left), PQE(right))

      case FormulaeType.OR =>
        val left: Formulae = f.asInstanceOf[OrFormulae].getLeftFormula
        //val leftType: FormulaeType.FormulaeType = left.getFormulaType
        val right: Formulae = f.asInstanceOf[OrFormulae].getRightFormula
        //val rightType: FormulaeType.FormulaeType = right.getFormulaType
        new OrFormulae(PQE(left), PQE(right))

      case FormulaeType.STRATEGIC =>
        val t1: Term = f.asInstanceOf[StrategicFormulae].getControllableAgent
        val t2: Term = f.asInstanceOf[StrategicFormulae].getUncontrollableAgent
        val nested: Formulae = f.asInstanceOf[StrategicFormulae].getNestedFormula
        //val nestedType: FormulaeType.FormulaeType = f.getFormulaType
        new StrategicFormulae(t1, t2, PQE(nested).asInstanceOf[PathFormulae])

      case FormulaeType.NEXT =>
        val nested: Formulae = f.asInstanceOf[NextFormulae].getRightFormula
        //val nestedType: FormulaeType.FormulaeType = f.getFormulaType
        new NextFormulae(PQE(nested))

      case FormulaeType.GLOBALLY =>
        val nested: Formulae = f.asInstanceOf[GloballyFormulae].getRightFormula
        //val nestedType: FormulaeType.FormulaeType = f.getFormulaType
        new GloballyFormulae(PQE(nested))

      case FormulaeType.UNTIL =>
        val left: Formulae = f.asInstanceOf[UntilFormulae].getLeftFormula
        //val leftType: FormulaeType.FormulaeType = f.getFormulaType
        val right: Formulae = f.asInstanceOf[UntilFormulae].getRightFormula
        //val rightType: FormulaeType.FormulaeType = f.getFormulaType
        new UntilFormulae(PQE(left), PQE(right))

      case FormulaeType.FORALL | FormulaeType.EXISTS | FormulaeType.FF | FormulaeType.FE | FormulaeType.EE | FormulaeType.EF =>
        val zero: Constant = new Constant(0)
        val y1: Variable = new Variable(1)
        val y2: Variable = new Variable(2)

        ftype match {
          case FormulaeType.FORALL =>
            val f1: ForallQuantifierFormulae = f.asInstanceOf[ForallQuantifierFormulae]
            val nested1 = f1.getNestedFormula
            val v1: Variable = f1.getQuantifiedVariable1
            if (nested1.getFormulaType == FormulaeType.STRATEGIC) {
              val nested1_1: StrategicFormulae = nested1.asInstanceOf[StrategicFormulae]
              val ca = nested1_1.getControllableAgent
              val unca = nested1_1.getUncontrollableAgent
              val nested2 = nested1_1.getNestedFormula
              if (v1 == y1 && ca == y1) {
                val nested: PathFormulae = PQE(nested2).asInstanceOf[PathFormulae]
                nested.substitute(v1, zero)
                new StrategicFormulae(zero, unca, nested)
              } else {
                new ForallQuantifierFormulae(PQE(nested1), v1)
              }
            } else {
              new ForallQuantifierFormulae(PQE(nested1), v1)
            }


          case FormulaeType.EXISTS =>
            val f1: ExistQuantifierFormulae = f.asInstanceOf[ExistQuantifierFormulae]
            val nested1 = f1.getNestedFormula
            val v1: Variable = f1.getQuantifiedVariable1
            if (nested1.getFormulaType == FormulaeType.STRATEGIC) {
              val nested1_1: StrategicFormulae = nested1.asInstanceOf[StrategicFormulae]
              val ca = nested1_1.getControllableAgent
              val unca = nested1_1.getUncontrollableAgent
              val nested2 = nested1_1.getNestedFormula
              if (v1 == y2 && unca == y2){
                val nested: PathFormulae = PQE(nested2).asInstanceOf[PathFormulae]
                nested.substitute(v1,zero)
                new StrategicFormulae(ca,zero,nested)
              } else {new ExistQuantifierFormulae(PQE(nested1),v1)}
            } else {new ExistQuantifierFormulae(PQE(nested1),v1)}


          case FormulaeType.FF =>
            val f1:FFQuantifierFormulae =f.asInstanceOf[FFQuantifierFormulae]
            val nested1 = f1.getNestedFormula
            val v1:Variable = f1.getQuantifiedVariable1
            val v2:Variable = f1.getQuantifiedVariable2
            if (nested1.getFormulaType == FormulaeType.STRATEGIC) {
              val nested1_1: StrategicFormulae = nested1.asInstanceOf[StrategicFormulae]
              val ca = nested1_1.getControllableAgent
              val unca = nested1_1.getUncontrollableAgent
              val nested2 = nested1_1.getNestedFormula
              if(v1 == y1 && v2 == y2 && ca == y1 && unca == y2) {
                val nested: PathFormulae = PQE(nested2).asInstanceOf[PathFormulae]
                nested.substitute(y1,zero)
                new ForallQuantifierFormulae(new StrategicFormulae(zero,y2,nested),y2)
              } else if(v1 == y2 && v2 == y1 && ca == y1 && unca == y2) {
                val nested: PathFormulae = PQE(nested2).asInstanceOf[PathFormulae]
                nested.substitute(y1,zero)
                new ForallQuantifierFormulae(new StrategicFormulae(zero,y2,nested),y2)
              } else {new FFQuantifierFormulae(PQE(nested1),v1,v2)}
            } else {new FFQuantifierFormulae(PQE(nested1),v1,v2)}


          case FormulaeType.EE =>
            val f1:EEQuantifierFormulae = f.asInstanceOf[EEQuantifierFormulae]
            val nested1 = f1.getNestedFormula
            val v1:Variable = f1.getQuantifiedVariable1
            val v2:Variable = f1.getQuantifiedVariable2
            if (nested1.getFormulaType == FormulaeType.STRATEGIC) {
              val nested1_1: StrategicFormulae = nested1.asInstanceOf[StrategicFormulae]
              val ca = nested1_1.getControllableAgent
              val unca = nested1_1.getUncontrollableAgent
              val nested2 = nested1_1.getNestedFormula
              if (v1 == y1 && v2 == y2 && ca == y1 && unca == y2) {
                val nested: PathFormulae = PQE(nested2).asInstanceOf[PathFormulae]
                nested.substitute(y2,zero)
                new ExistQuantifierFormulae(new StrategicFormulae(y1,zero,nested),y1)
              } else if (v1 == y2 && v2 == y1 && ca == y1 && unca == y2) {
                val nested: PathFormulae = PQE(nested2).asInstanceOf[PathFormulae]
                nested.substitute(y2,zero)
                new ExistQuantifierFormulae(new StrategicFormulae(y1,zero,nested),y1)
              } else {new EEQuantifierFormulae(PQE(nested1),v1,v2)}
            } else {new EEQuantifierFormulae(PQE(nested1),v1,v2)}


          case FormulaeType.EF =>
            val f1:EFQuantifierFormulae = f.asInstanceOf[EFQuantifierFormulae]
            val nested1 = f1.getNestedFormula
            val v1:Variable = f1.getQuantifiedVariable1
            val v2:Variable = f1.getQuantifiedVariable2
            if (nested1.getFormulaType == FormulaeType.STRATEGIC) {
              val nested1_1: StrategicFormulae = nested1.asInstanceOf[StrategicFormulae]
              val ca = nested1_1.getControllableAgent
              val unca = nested1_1.getUncontrollableAgent
              val nested2 = nested1_1.getNestedFormula
              if (v1 == y2 && v2 == y1 && ca == y1 && unca == y2) {
                val nested: PathFormulae = PQE(nested2).asInstanceOf[PathFormulae]
                nested.substitute(y1,zero)
                nested.substitute(y2,zero)
                new StrategicFormulae(zero,zero,nested)
              } else {new EFQuantifierFormulae(PQE(nested1),v1,v2)}
            } else {new EFQuantifierFormulae(PQE(nested1),v1,v2)}


          case FormulaeType.FE =>
            val f1:FEQuantifierFormulae = f.asInstanceOf[FEQuantifierFormulae]
            val nested1 = f1.getNestedFormula
            val v1:Variable = f1.getQuantifiedVariable1
            val v2:Variable = f1.getQuantifiedVariable2
            if (nested1.getFormulaType == FormulaeType.STRATEGIC) {
              val nested1_1: StrategicFormulae = nested1.asInstanceOf[StrategicFormulae]
              val ca = nested1_1.getControllableAgent
              val unca = nested1_1.getUncontrollableAgent
              val nested2 = nested1_1.getNestedFormula
              if(v1 == y1 && v2 == y2 && ca == y1 && unca == y2){
                val nested: PathFormulae = PQE(nested2).asInstanceOf[PathFormulae]
                nested.substitute(y1,zero)
                nested.substitute(y2,zero)
                new StrategicFormulae(zero,zero,nested)
              } else {new FEQuantifierFormulae(PQE(nested1),v1,v2)}
            } else {new FEQuantifierFormulae(PQE(nested1),v1,v2)}

          case _ => throw new RuntimeException("Unknown formula type")
        }
    }
  }



  def PUSH(q:QuantifierType.QuantifierType,qv1:Variable,qv2:Variable,f:Formulae):Formulae = {
    //qv1 qv2 means position
    val y1: Variable = new Variable(1)
    val y2: Variable = new Variable(2)
    f.getFormulaType match {
      case FormulaeType.TRUE_ATOM => f

      case FormulaeType.ATOMIC => f

      case FormulaeType.NOT =>
        val f1: NotFormulae = f.asInstanceOf[NotFormulae]
        val nested: Formulae = f1.getNestedFormula
        q match {
          case QuantifierType.FORALL => new NotFormulae(PUSH(QuantifierType.EXISTS,qv1,qv2, nested))
          case QuantifierType.EXISTS => new NotFormulae(PUSH(QuantifierType.FORALL,qv1,qv2, nested))
          case QuantifierType.FF => new NotFormulae(PUSH(QuantifierType.EE,qv1,qv2, nested))
          case QuantifierType.EE => new NotFormulae(PUSH(QuantifierType.FF,qv1,qv2, nested))
          case QuantifierType.EF => new NotFormulae(PUSH(QuantifierType.FE,qv1,qv2, nested))
          case QuantifierType.FE => new NotFormulae(PUSH(QuantifierType.EF,qv1,qv2, nested))
        }

      case FormulaeType.AND =>
        val f1: AndFormulae = f.asInstanceOf[AndFormulae]
        val left: Formulae = f1.getLeftFormula
        val right: Formulae = f1.getRightFormula
        new AndFormulae(PUSH(q,qv1,qv2, left), PUSH(q,qv1,qv2, right))

      case FormulaeType.OR =>
        val f1: OrFormulae = f.asInstanceOf[OrFormulae]
        val left: Formulae = f1.getLeftFormula
        val right: Formulae = f1.getRightFormula
        new OrFormulae(PUSH(q,qv1,qv2, left), PUSH(q,qv1,qv2, right))

      case FormulaeType.NEXT =>
        val f1: NextFormulae = f.asInstanceOf[NextFormulae]
        val nested: Formulae = f1.getRightFormula
        new NextFormulae(PUSH(q,qv1,qv2, nested))

      case FormulaeType.GLOBALLY =>
        val f1:GloballyFormulae = f.asInstanceOf[GloballyFormulae]
        val nested:Formulae =f1.getRightFormula
        new GloballyFormulae(PUSH(q,qv1,qv2,nested))

      case FormulaeType.UNTIL =>
        val f1:UntilFormulae = f.asInstanceOf[UntilFormulae]
        val left:Formulae = f1.getLeftFormula
        val right:Formulae = f1.getRightFormula
        new UntilFormulae(PUSH(q,qv1,qv2,left),PUSH(q,qv1,qv2,right))

      case FormulaeType.STRATEGIC =>
        val f1:StrategicFormulae = f.asInstanceOf[StrategicFormulae]
        val ca = f1.getControllableAgent
        val unca = f1.getUncontrollableAgent
        val nested:PathFormulae = f1.getNestedFormula
        val newNested:PathFormulae = PUSH(q, qv1, qv2, nested).asInstanceOf[PathFormulae]
        q match {
          case QuantifierType.FORALL =>
            if (qv1 == y1){
              if (ca == y1){
                new ForallQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),qv1)
              } else { new StrategicFormulae(ca, unca, newNested)}
            } else if(qv1 == y2){
              if (unca == y2){
                new ForallQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),qv1)
              } else { new StrategicFormulae(ca, unca, newNested)}
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case QuantifierType.EXISTS =>
            if (qv1 == y1){
              if (ca == y1){
                new ExistQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),qv1)
              } else { new StrategicFormulae(ca, unca, newNested)}
            } else if(qv1 == y2){
              if (unca == y2){
                new ExistQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),qv1)
              } else { new StrategicFormulae(ca, unca, newNested)}
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case QuantifierType.FF =>
            if ((qv1 == y1 && qv2 == y2) || (qv1 == y2 && qv2 == y1)){
              if (ca == y1) {
                if (unca == y2){
                  new FFQuantifierFormulae(new StrategicFormulae(ca, unca, newNested),qv1,qv2)
                } else {new ForallQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y1)}
              } else {
                if (unca == y2){
                  new ForallQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y2)
                } else { new StrategicFormulae(ca,unca,newNested)}
              }
            } else {
              throw new RuntimeException("Incorrect quantified variables")}

          case QuantifierType.EE =>
            if ((qv1 == y1 && qv2 == y2) || (qv1 == y2 && qv2 == y1)){
              if (ca == y1){
                if (unca == y2) {
                  new EEQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),qv1,qv2)
                } else {new ExistQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y1)}
              }else {
                if (unca == y2){
                  new ExistQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y2)
                } else { new StrategicFormulae(ca,unca,newNested)}
              }
            } else {
                throw new RuntimeException("Incorrect quantified variables")
              }

          case QuantifierType.EF =>
            if(qv1 == y1 && qv2 == y2){
              if (ca == y1){
                if (unca == y2){
                  new EFQuantifierFormulae(new StrategicFormulae(ca,unca, newNested),qv1,qv2)
                } else{new ExistQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y1)}
              } else {
                if (unca == y2){
                  new ForallQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y2)
                }else{new StrategicFormulae(ca,unca,newNested)}
              }
            }else if(qv1 == y2 && qv2 == y1){
              if (ca == y1){
                if (unca == y2){
                  new EFQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),qv1,qv2)
                } else{new ForallQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y1)}
              } else{
                if (unca == y2){
                  new ExistQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y2)
                } else {new StrategicFormulae(ca,unca,newNested)}
              }
            }else {throw new RuntimeException("Incorrect quantified variables")}

          case QuantifierType.FE =>
            if(qv1 == y1 && qv2 == y2){
              if (ca == y1){
                if (unca == y2){
                  new FEQuantifierFormulae(new StrategicFormulae(ca,unca, newNested),qv1,qv2)
                } else{new ForallQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y1)}
              } else {
                if (unca == y2){
                  new ExistQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y2)
                }else{new StrategicFormulae(ca,unca,newNested)}
              }
            }else if(qv1 == y2 && qv2 == y1){
              if (ca == y1){
                if (unca == y2){
                  new FEQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),qv1,qv2)
                } else{new ExistQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y1)}
              } else{
                if (unca == y2){
                  new ForallQuantifierFormulae(new StrategicFormulae(ca,unca,newNested),y2)
                } else {new StrategicFormulae(ca,unca,newNested)}
              }
            }else {throw new RuntimeException("Incorrect quantified variables")}
        }

      case FormulaeType.FORALL =>
        val f1:ForallQuantifierFormulae = f.asInstanceOf[ForallQuantifierFormulae]
        val v:Variable = f1.getQuantifiedVariable1
        val nested:Formulae = f1.getNestedFormula
        q match {
          case QuantifierType.FORALL =>
            if (qv1 == v){
              PUSH(QuantifierType.FORALL,v,null,nested)
            } else {PUSH(QuantifierType.FF,qv1,v,nested)}

          case QuantifierType.EXISTS =>
            if (qv1 == v){
              PUSH(QuantifierType.EXISTS,v,null,nested)
            } else {PUSH(QuantifierType.EF,qv1,v,nested)}

          case QuantifierType.EE =>
            if (v == qv1) {
              PUSH(QuantifierType.EF,qv2,v,nested)
            } else if(v == qv2) {
              PUSH(QuantifierType.EF,qv1,v,nested)
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case QuantifierType.FF =>
            if (v == qv1) {
              PUSH(QuantifierType.FF,qv2,v,nested)
            } else if(v == qv2) {
              PUSH(QuantifierType.FF,qv1,v,nested)
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case QuantifierType.EF =>
            if (v == qv1){
              PUSH(QuantifierType.FF,qv2,v,nested)
            } else if(v == qv2){
              PUSH(QuantifierType.EF,qv1,v,nested)
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case QuantifierType.FE =>
            if (v == qv1){
              PUSH(QuantifierType.EF,qv2,v,nested)
            } else if(v == qv2){
              PUSH(QuantifierType.FF,qv1,v,nested)
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case _ => throw new RuntimeException("Incorrect quantifier type")
        }

      case FormulaeType.EXISTS =>
        val f1:ExistQuantifierFormulae = f.asInstanceOf[ExistQuantifierFormulae]
        val v:Variable = f1.getQuantifiedVariable1
        val nested:Formulae = f1.getNestedFormula
        q match {
          case QuantifierType.FORALL =>
            if (qv1 == v){
              PUSH(QuantifierType.EXISTS,v,null,nested)
            } else {PUSH(QuantifierType.FE,qv1,v,nested)}

          case QuantifierType.EXISTS =>
            if (qv1 == v){
              //println(nested)
              PUSH(QuantifierType.EXISTS,v,null,nested)
            } else {PUSH(QuantifierType.EE,qv1,v,nested)}

          case QuantifierType.EE =>
            if (v == qv1) {
              PUSH(QuantifierType.EE,qv2,v,nested)
            } else if(v == qv2) {
              PUSH(QuantifierType.EE,qv1,v,nested)
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case QuantifierType.FF =>
            if (v == qv1) {
              PUSH(QuantifierType.FE,qv2,v,nested)
            } else if(v == qv2) {
              PUSH(QuantifierType.FE,qv1,v,nested)
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case QuantifierType.EF =>
            if (v == qv1){
              PUSH(QuantifierType.FE,qv2,v,nested)
            } else if(v == qv2){
              PUSH(QuantifierType.EE,qv1,v,nested)
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case QuantifierType.FE =>
            if (v == qv1){
              PUSH(QuantifierType.EE,qv2,v,nested)
            } else if(v == qv2){
              PUSH(QuantifierType.FE,qv1,v,nested)
            } else {throw new RuntimeException("Incorrect quantified variable")}

          case _ => throw new RuntimeException("Incorrect quantifier type")
        }

      case FormulaeType.FF =>
        val f1:FFQuantifierFormulae = f.asInstanceOf[FFQuantifierFormulae]
        val v1:Variable = f1.getQuantifiedVariable1
        val v2:Variable = f1.getQuantifiedVariable2
        val nested:Formulae = f1.getNestedFormula
        val newNested:Formulae = new ForallQuantifierFormulae(new ForallQuantifierFormulae(nested,v2),v1)
        PUSH(q, qv1, qv2, newNested)

      case FormulaeType.EE =>
        val f1:EEQuantifierFormulae = f.asInstanceOf[EEQuantifierFormulae]
        val v1:Variable = f1.getQuantifiedVariable1
        val v2:Variable = f1.getQuantifiedVariable2
        val nested:Formulae = f1.getNestedFormula
        val newNested:Formulae = new ExistQuantifierFormulae(new ExistQuantifierFormulae(nested,v2),v1)
        PUSH(q, qv1, qv2, newNested)

      case FormulaeType.FE =>
        val f1:FEQuantifierFormulae = f.asInstanceOf[FEQuantifierFormulae]
        val v1:Variable = f1.getQuantifiedVariable1
        val v2:Variable = f1.getQuantifiedVariable2
        val nested:Formulae = f1.getNestedFormula
        val newNested:Formulae = new ForallQuantifierFormulae(new ExistQuantifierFormulae(nested,v2),v1)
        PUSH(q, qv1, qv2, newNested)

      case FormulaeType.EF =>
        val f1:EFQuantifierFormulae = f.asInstanceOf[EFQuantifierFormulae]
        val v1:Variable = f1.getQuantifiedVariable1
        val v2:Variable = f1.getQuantifiedVariable2
        val nested:Formulae = f1.getNestedFormula
        val newNested:Formulae = new ExistQuantifierFormulae(new ForallQuantifierFormulae(nested,v2),v1)
        PUSH(q, qv1, qv2, newNested)

      case _ => throw new RuntimeException("Incorrect formula type")
    }
  }

  def NF(f:Formulae):Formulae = f.getFormulaType match {
    case FormulaeType.TRUE_ATOM => f

    case FormulaeType.ATOMIC => f

    case FormulaeType.NOT =>
      val f1:NotFormulae = f.asInstanceOf[NotFormulae]
      val nested:Formulae = f1.getNestedFormula
      new NotFormulae(NF(nested))

    case FormulaeType.AND =>
      val f1:AndFormulae = f.asInstanceOf[AndFormulae]
      val left:Formulae = f1.getLeftFormula
      val right:Formulae = f1.getRightFormula
      new AndFormulae(NF(left),NF(right))

    case FormulaeType.OR =>
      val f1:OrFormulae = f.asInstanceOf[OrFormulae]
      val left:Formulae = f1.getLeftFormula
      val right:Formulae = f1.getRightFormula
      new OrFormulae(NF(left),NF(right))

    case FormulaeType.STRATEGIC =>
      val f1:StrategicFormulae = f.asInstanceOf[StrategicFormulae]
      val ca = f1.getControllableAgent
      val unca = f1.getUncontrollableAgent
      val nested:PathFormulae = f1.getNestedFormula
      new StrategicFormulae(ca,unca,NF(nested).asInstanceOf[PathFormulae])

    case FormulaeType.NEXT =>
      val f1:NextFormulae = f.asInstanceOf[NextFormulae]
      val nested:Formulae = f1.getRightFormula
      new NextFormulae(NF(nested))

    case FormulaeType.GLOBALLY =>
      val f1:GloballyFormulae = f.asInstanceOf[GloballyFormulae]
      val nested:Formulae = f1.getRightFormula
      new GloballyFormulae(NF(nested))

    case FormulaeType.UNTIL =>
      val f1:UntilFormulae = f.asInstanceOf[UntilFormulae]
      val left:Formulae = f1.getLeftFormula
      val right:Formulae =f1.getRightFormula
      new UntilFormulae(NF(left),NF(right))

    case FormulaeType.FORALL =>
      val f1:ForallQuantifierFormulae = f.asInstanceOf[ForallQuantifierFormulae]
      val v:Variable = f1.getQuantifiedVariable1
      val nested:Formulae = f1.getNestedFormula
      //println(nested.toString)
      PQE(PUSH(QuantifierType.FORALL,v,null,NF(nested)))

    case FormulaeType.EXISTS =>
      val f1:ExistQuantifierFormulae = f.asInstanceOf[ExistQuantifierFormulae]
      val v:Variable = f1.getQuantifiedVariable1
      val nested:Formulae =f1.getNestedFormula
      //println(nested.toString)
      PQE(PUSH(QuantifierType.EXISTS,v,null,NF(nested)))

    case FormulaeType.FF =>
      val f1:FFQuantifierFormulae =f.asInstanceOf[FFQuantifierFormulae]
      val v1:Variable = f1.getQuantifiedVariable1
      val v2:Variable = f1.getQuantifiedVariable2
      val nested:Formulae = f1.getNestedFormula
      PQE(PUSH(QuantifierType.FF,v1,v2,NF(nested)))

    case FormulaeType.EE =>
      val f1:EEQuantifierFormulae = f.asInstanceOf[EEQuantifierFormulae]
      val v1:Variable = f1.getQuantifiedVariable1
      val v2:Variable = f1.getQuantifiedVariable2
      val nested:Formulae = f1.getNestedFormula
      PQE(PUSH(QuantifierType.EE,v1,v2,NF(nested)))

    case FormulaeType.FE =>
      val f1:FEQuantifierFormulae = f.asInstanceOf[FEQuantifierFormulae]
      val v1:Variable = f1.getQuantifiedVariable1
      val v2:Variable = f1.getQuantifiedVariable2
      val nested:Formulae = f1.getNestedFormula
      PQE(PUSH(QuantifierType.FE,v1,v2,NF(nested)))

    case FormulaeType.EF =>
      val f1:EFQuantifierFormulae = f.asInstanceOf[EFQuantifierFormulae]
      val v1:Variable = f1.getQuantifiedVariable1
      val v2:Variable = f1.getQuantifiedVariable2
      val nested:Formulae = f1.getNestedFormula
      PQE(PUSH(QuantifierType.EF,v1,v2,NF(nested)))

    case _ => throw new RuntimeException("Incorrect formula type")

  }


}

object Function{
  def main(args: Array[String]): Unit ={
    val y1: Variable = new Variable(1)
    val y2: Variable = new Variable(2)
    //val name1 = new Variable(3)
    //val pName1 = new Predicate("p1",1)
    //val p1 = new Atomic(pName1,name1)
    val p1 = new AtomicProposition("p1")
    //val name2 = new Variable(4)
    //val pName2 = new Predicate("p2",1)
    //val p2 = new Atomic(pName2,name2)
    val p2 = new AtomicProposition("p2")
    //val name3 = new Variable(5)
    //val pName3 = new Predicate("p3",1)
    //val p3 = new Atomic(pName3,name3)
    val p3 = new AtomicProposition("p3")
    val part1_1_1 = new NextFormulae(p1)
    val part1_1_2 = new StrategicFormulae(y1,y2,part1_1_1)
    val part1_1_3 = new ForallQuantifierFormulae(part1_1_2,y2)
    val part1_2_1 = new UntilFormulae(new TrueAtom,p2)
    val part1_2_2 = new StrategicFormulae(y1,y2,part1_2_1)
    val part1_2_3 = new ExistQuantifierFormulae(part1_2_2,y2)
    val part1 = new StrategicFormulae(y1,new Constant(5),new UntilFormulae(part1_1_3,part1_2_3))
    val part2_1_1 = new StrategicFormulae(y1,y2,new UntilFormulae(new TrueAtom,p3))
    val part2_1_2 = new ForallQuantifierFormulae(part2_1_1,y2)
    val part2_2_1 = new StrategicFormulae(new Constant(3),y2,new NextFormulae(p1))
    val part2_2_2 = new ForallQuantifierFormulae(part2_2_1,y2)
    val part2_2_3 = new NotFormulae(part2_2_2)
    val part2 = new ExistQuantifierFormulae(new AndFormulae(part2_1_2,part2_2_3),y1)
    val formula = new ForallQuantifierFormulae(new OrFormulae(part1,part2),y1)
    val f = new Function

    //println(f.PUSH(QuantifierType.FORALL,y1,null,formula).toString)

    println("*************")

    //println(f.PUSH(QuantifierType.FORALL,y1,null,f.NF(new)).toString)

    //println("*************")

    val A = f.NF(part1)

    //println(f.NF(part1).toString)

    //println("*************")

    val B = f.NF(part2)

    //println(f.NF(part2).toString)

    //println("*************")

    //println(f.PUSH(QuantifierType.FORALL,y1,null,new OrFormulae(A,B)))

    println(f.NF(formula))




  }


}


