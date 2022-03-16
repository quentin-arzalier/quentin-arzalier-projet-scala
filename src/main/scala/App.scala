import scala.annotation.tailrec

object App{
  enum NamedLambda{
    case variable(name: String)
    case abstraction(name: String, result: NamedLambda)
    case application(function: NamedLambda, argument: NamedLambda)
  }

  enum BruijnLambda{
    case variableLiee(index: Int)
    case variableLibre(nom: String)
    case abstraction(result: BruijnLambda)
    case application(function: BruijnLambda, argument: BruijnLambda)
  }

  def namedToString(namedLambda: NamedLambda): String =
    namedLambda match {
      case NamedLambda.variable(name) => name
      case NamedLambda.abstraction(name, result) => result match {
        case NamedLambda.application(_,_) => "λ" + name + ".(" + namedToString(result) + ")"
        case _ => "λ" + name + "." + namedToString(result)
      }
      case NamedLambda.application(function, argument) => argument match {
        case NamedLambda.abstraction(_,_) => namedToString(function) + " (" + namedToString(argument) + ")"
        case _ => namedToString(function) + " " + namedToString(argument)
      }
    }
  def bruijnToString(bruijnLambda: BruijnLambda): String =
    bruijnLambda match {
      case BruijnLambda.variableLiee(int) => int.toString
      case BruijnLambda.variableLibre(nom) => nom
      case BruijnLambda.abstraction(result) => result match {
        case BruijnLambda.application(_,_) => "λ.(" + bruijnToString(result) + ")"
        case _ => "λ." + bruijnToString(result)
      }
      case BruijnLambda.application(function, argument) => argument match {
        case BruijnLambda.abstraction(_) => bruijnToString(function) + " (" + bruijnToString(argument) + ")"
        case _ => bruijnToString(function) + " " + bruijnToString(argument)
      }
    }

  def findNameInList(name: String, list: List[String]): BruijnLambda =
    @tailrec
    def aux(name: String, list: List[String], n: Int): BruijnLambda =
      list match {
        case x :: reste => if (x == name) BruijnLambda.variableLiee(n) else aux(name, reste, n+1)
        case Nil => BruijnLambda.variableLibre(name)
      }
    aux(name, list, 0)

  def namedToBruijn(namedLambda: NamedLambda): BruijnLambda =
    def aux(namedAux: NamedLambda, names: List[String]): BruijnLambda =
      namedAux match {
        case NamedLambda.variable(name) => findNameInList(name, names)
        case NamedLambda.abstraction(name, result) => BruijnLambda.abstraction(aux(result, name :: names))
        case NamedLambda.application(function, argument) => BruijnLambda.application(aux(function, names), aux(argument, names))
      }
    aux(namedLambda, Nil)


  def estFormeNormale(bruijnLambda: BruijnLambda): Boolean =
    etapeReductionParesseuse(bruijnLambda).equals(bruijnLambda)

  @tailrec
  def reductionParesseuseTotale(bruijnLambda: BruijnLambda): BruijnLambda =
    if (estFormeNormale(bruijnLambda)) bruijnLambda else reductionParesseuseTotale(etapeReductionParesseuse(bruijnLambda))

  def etapeReductionParesseuse(bruijnLambda: BruijnLambda): BruijnLambda =
    bruijnLambda match {
      case BruijnLambda.application(function, argument) => function match {
        case BruijnLambda.abstraction(result) => replaceAll(function, argument, 0)
        case BruijnLambda.application(_,_) => BruijnLambda.application(etapeReductionParesseuse(function), argument)
        case _ => bruijnLambda
      }
      case _ => bruijnLambda
    }

  def nbLambdasDansTerme(terme: BruijnLambda): Int =
    terme match {
      case BruijnLambda.abstraction(result) => 1 + nbLambdasDansTerme(result)
      case BruijnLambda.application(function, argument) => nbLambdasDansTerme(function) + nbLambdasDansTerme(argument)
      case _ => 0
    }
  def replaceAll(abstraction: BruijnLambda, remplacant: BruijnLambda, pos: Int): BruijnLambda =
    abstraction match {
      case BruijnLambda.abstraction(result) => result match {
        case BruijnLambda.variableLiee(index) => if (index == pos) remplacant else result
        case BruijnLambda.variableLibre(nom) => result
        case BruijnLambda.abstraction(_) => BruijnLambda.abstraction(replaceAll(result, remplacant, pos+1))
        case BruijnLambda.application(function, argument) => BruijnLambda.application(replaceAll(function, remplacant, pos), replaceAll(argument, remplacant, pos + nbLambdasDansTerme(function)))
      }
      case _ => abstraction
    }


  @main def main(): Unit =
    val u = NamedLambda.variable("u")
    val v = NamedLambda.variable("v")
    val w = NamedLambda.variable("w")
    val x = NamedLambda.variable("x")
    val y = NamedLambda.variable("y")
    val z = NamedLambda.variable("z")

    val appli1 = NamedLambda.application(y, v)
    val abstract1 = NamedLambda.abstraction("v", appli1)
    val appli2 = NamedLambda.application(y, abstract1)
    val appli3 = NamedLambda.application(appli2, u)
    val abstract2 = NamedLambda.abstraction("z", appli3)
    val abstract3 = NamedLambda.abstraction("y", abstract2)
    val abstract4 = NamedLambda.abstraction("x", abstract3)
    val bruijnLambda1 = namedToBruijn(abstract4)
    println(namedToString(abstract4))
    println(bruijnToString(bruijnLambda1))
    println(bruijnToString(reductionParesseuseTotale(bruijnLambda1)))

    println("\n-----------\n")

    val abstract5 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", x))
    val appli4 = NamedLambda.application(abstract5, u)
    val appli5 = NamedLambda.application(appli4, v)
    val bruijnLambda2 = namedToBruijn(appli5)
    println(namedToString(appli5))
    println(bruijnToString(bruijnLambda2))
    println(bruijnToString(reductionParesseuseTotale(bruijnLambda2)))

    println("\n-----------\n")

    val c_appli1 = NamedLambda.application(u, x)
    val c_abstr1 = NamedLambda.abstraction("u", c_appli1)
    val c_appli2 = NamedLambda.application(z,x)
    val c_appli3 = NamedLambda.application(c_appli2, c_abstr1)
    val c_abstr2 = NamedLambda.abstraction("y", c_appli3)
    val c_abstr3 = NamedLambda.abstraction("x", c_abstr2)
    val c_appli4 = NamedLambda.application(w, x)
    val c_abstr4 = NamedLambda.abstraction("x", c_appli4)
    val c_final = NamedLambda.application(c_abstr3, c_abstr4)
    val bruijnLambda3 = namedToBruijn(c_final)
    println(namedToString(c_final))
    println(bruijnToString(bruijnLambda3))
    println(bruijnToString(reductionParesseuseTotale(bruijnLambda3)))



}

