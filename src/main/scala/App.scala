object App:

  enum LambdaTerme{
    case normal(nom: String)
    case abstraction(abstraction: LambdaTerme => LambdaTerme)
    case application(function: LambdaTerme, argument: LambdaTerme)
  }

  def estFormeNormale(terme: LambdaTerme): Boolean = terme match {
    case LambdaTerme.normal(_) => true
    case _ => false;
  }

  def namedtoString(terme: LambdaTerme): String =
    def aux(terme: LambdaTerme, i: Int): String =
      terme match {
        case LambdaTerme.normal(nom) => nom;
        case LambdaTerme.application(x1, x2) => aux(x1, i) + " " + aux(x2, i);
        case LambdaTerme.abstraction(f) => "(λx" + i.toString + "." + aux(f(LambdaTerme.normal("x" + i.toString)), i+1) + ")";
      }
    aux(terme, 1)

  def evaluer(terme: LambdaTerme): LambdaTerme = terme match {
    case LambdaTerme.application(t, x) => t match {
      case LambdaTerme.application(_, _) => LambdaTerme.application(evaluer(t), x)
      case LambdaTerme.abstraction(f) => f(x)
    }
    case _ => terme
  }


  @main def main(): Unit =
    println("C'est parti")
    val lambda1 = LambdaTerme.normal("x")
    val lambda2 = LambdaTerme.normal("y")
    val abstract1 = LambdaTerme.abstraction((x: LambdaTerme) => LambdaTerme.abstraction((y: LambdaTerme) => x))
    val appli1 = LambdaTerme.application(abstract1, lambda1)
    val appli2 = LambdaTerme.application(appli1, lambda2)
    println(namedtoString(appli2))
    println(namedtoString(evaluer(appli2)))
