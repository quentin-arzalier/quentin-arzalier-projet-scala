import scala.annotation.tailrec

object App{

  /**
   * Une expression de lambda calcul utilisant des variables nommées
   */
  enum NamedLambda{
    case variable(name: String)
    case abstraction(name: String, result: NamedLambda)
    case application(function: NamedLambda, argument: NamedLambda)
  }

  /**
   * Une expression de lambda calcul utilisant des indices de Bruijn
   */
  enum BruijnLambda{
    case variableLiee(index: Int)
    case variableLibre(nom: String)
    case abstraction(result: BruijnLambda)
    case application(function: BruijnLambda, argument: BruijnLambda)
  }

  /**
   * Cette méthode transforme une expression nommée en une expression avec indices de bruijn
   * @param namedLambda L'expression nommée
   * @return Une expression équivalente avec indices de bruijn
   */
  def namedToString(namedLambda: NamedLambda): String =
    namedLambda match {
      case NamedLambda.variable(name) => name
      case NamedLambda.abstraction(name, result) => result match {
        case NamedLambda.application(_,_) => "λ" + name + ".(" + namedToString(result) + ")"
        case _ => "λ" + name + "." + namedToString(result)
      }
      case NamedLambda.application(function, argument) => "(" + (argument match {
        case NamedLambda.abstraction(_,_) => namedToString(function) + " (" + namedToString(argument) + ")"
        case _ => namedToString(function) + " " + namedToString(argument)
      }) + ")"
    }


  /**
   * Transforme une expression utilisant les indices de bruijn en une chaîne de caractères affichable
   * @param bruijnLambda L'expression utilisant les indices de bruijn
   * @return Une chaîne de caractères correspondant à l'expression
   */
  def bruijnToString(bruijnLambda: BruijnLambda): String =
    bruijnLambda match {
      case BruijnLambda.variableLiee(int) => int.toString
      case BruijnLambda.variableLibre(nom) => nom
      case BruijnLambda.abstraction(result) => result match {
        case BruijnLambda.application(_,_) => "λ.(" + bruijnToString(result) + ")"
        case _ => "λ." + bruijnToString(result)
      }
      case BruijnLambda.application(function, argument) => "(" + (argument match {
        case BruijnLambda.abstraction(_) => bruijnToString(function) + " (" + bruijnToString(argument) + ")"
        case _ => bruijnToString(function) + " " + bruijnToString(argument)
      }) + ")"
    }


  /**
   * Permet de transformer une variable nommée en une variable de bruijn
   * @param name Le nom de la variable nommée
   * @param list La liste contenant tous les noms de variables utilisés dans l'expression nommée
   * @return Une variable liée ou libre utilisant les indices de bruijn avec l'indice correct
   */
  def findNameInList(name: String, list: List[String]): BruijnLambda =
    @tailrec
    def aux(name: String, list: List[String], n: Int): BruijnLambda =
      list match {
        case x :: reste => if (x == name) BruijnLambda.variableLiee(n) else aux(name, reste, n+1)
        case Nil => BruijnLambda.variableLibre(name)
      }
    aux(name, list, 0)


  /**
   * Transforme une expression utilisant des variables nommées en une expression utilisant des indices de Bruijn
   * @param namedLambda L'expression utilisant des variables nommées
   * @return L'expression utilisant des indices de Bruijn
   */
  def namedToBruijn(namedLambda: NamedLambda): BruijnLambda =
    def aux(namedAux: NamedLambda, names: List[String]): BruijnLambda =
      namedAux match {
        case NamedLambda.variable(name) => findNameInList(name, names)
        case NamedLambda.abstraction(name, result) => BruijnLambda.abstraction(aux(result, name :: names))
        case NamedLambda.application(function, argument) => BruijnLambda.application(aux(function, names), aux(argument, names))
      }
    aux(namedLambda, Nil)

  /**
   * Transforme une expression utilisant des indices de Bruijn en une expression utilisant des variables nommées
   * @param bruijnLambda L'expression utilisant des indices de Bruijn
   * @return L'expression utilisant des variables nommées
   */
  def bruijnToNamed(bruijnLambda: BruijnLambda): NamedLambda =
    def aux(bruijnLambda: BruijnLambda, nbVars: Int, names: List[String]): NamedLambda =
      bruijnLambda match {
        case BruijnLambda.variableLiee(index) => NamedLambda.variable(getNthElementOfList(index, names))
        case BruijnLambda.variableLibre(nom) => NamedLambda.variable(nom)
        case BruijnLambda.abstraction(result) => NamedLambda.abstraction("x"+nbVars,aux(result, nbVars+1, "x"+nbVars :: names))
        case BruijnLambda.application(function, argument) => NamedLambda.application(aux(function, nbVars, names), aux(argument, nbVars+nbLambdasDansTerme(function), names))
      }
    aux(bruijnLambda, 1, Nil)


  /**
   * Permet de récupérer le n-ième élément d'une liste
   * @param n La position dans la liste de l'objet recherché
   * @param list La liste à parcourir
   * @tparam A Le type d'éléments de la liste
   * @return L'élément en position n de la liste
   */
  @tailrec
  def getNthElementOfList[A](n: Int, list: List[A]): A =
    if (n==0) list.head else getNthElementOfList(n-1, list.tail);

  /**
   * Permet de savoir si une expression est en forme normale (autrement dit, si on ne peut pas la réduire)
   * @param lambda L'expression à vérifier
   * @return Un booléen disant si l'expression donnée est bien en forme normale
   */
  @tailrec
  def estFormeNormale[A](lambda: A): Boolean = lambda match {
    case lambda: BruijnLambda => etapeReductionParesseuse(lambda).equals(lambda)
    case lambda: NamedLambda => estFormeNormale(namedToBruijn(lambda))
    case _ => throw new IllegalArgumentException
  }

  /**
   * Réalise une réduction totale d'une expression utilisant des variables nommées en passant par les indices de Bruijn
   * @param namedLambda L'expression à réduire
   * @return Une expression totalement réduite de l'expression donnée (le nom des variables liées changera)
   */
  def reductionParesseuseTotale(namedLambda: NamedLambda): NamedLambda =
    bruijnToNamed(reductionParesseuseTotale(namedToBruijn(namedLambda)))

  /**
   * Réalise une réduction totale d'une expression utilisant les indices de Bruijn
   * @param bruijnLambda L'expression à réduire
   * @return Une expression totalement réduite de l'expression donnée
   */
  @tailrec
  def reductionParesseuseTotale(bruijnLambda: BruijnLambda): BruijnLambda =
    if (estFormeNormale(bruijnLambda)) bruijnLambda else reductionParesseuseTotale(etapeReductionParesseuse(bruijnLambda))

  /**
   * Réalise une unique étape de réduction paresseuse
   * @param bruijnLambda L'expression à réduire
   * @return L'expression à réduire après une étape de beta réduction paresseuse
   */
  def etapeReductionParesseuse(bruijnLambda: BruijnLambda): BruijnLambda =
    bruijnLambda match {
      case BruijnLambda.application(function, argument) => function match {
        case BruijnLambda.abstraction(result) => replaceAll(function, argument, 0)
        case BruijnLambda.application(_,_) => BruijnLambda.application(etapeReductionParesseuse(function), argument)
        case _ => argument match {
          case BruijnLambda.abstraction(result) => replaceAll(function, argument, 0)
          case BruijnLambda.application(_, _) => BruijnLambda.application(etapeReductionParesseuse(function), argument)
          case _ => bruijnLambda
        }
      }
      case BruijnLambda.abstraction(result) => BruijnLambda.abstraction(etapeReductionParesseuse(result))
      case _ => bruijnLambda
    }

  /**
   * Compte le nombre de lambdas présents dans un terme (pour les indices)
   * @param terme Le terme à évaluer
   * @return Le nombre d'abstractions dans le terme
   */
  def nbLambdasDansTerme(terme: BruijnLambda): Int =
    terme match {
      case BruijnLambda.abstraction(result) => 1 + nbLambdasDansTerme(result)
      case BruijnLambda.application(function, argument) => nbLambdasDansTerme(function) + nbLambdasDansTerme(argument)
      case _ => 0
    }


  /**
   * Remplace tous les bons termes dans une abstraction par une autre expression
   * @param abstraction L'expression à parcourir
   * @param remplacant L'expression qui remplacera les bons indices
   * @param pos L'indice de Bruijn actuel (incrémente pour chaque abstraction rencontrée)
   * @return L'expression donnée en paramère avec le remplacement effectué
   */
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

  /**
   * Compte le nombre d'éléments dans une expression utilisant des variable nommées
   * @param namedLambda L'expression à évaluer
   * @return Le nombre d'éléments dans l'expression
   */
  def nbElementsDansNamed(namedLambda: NamedLambda): Int =
    namedLambda match {
      case NamedLambda.variable(name) => 1
      case NamedLambda.abstraction(name, result) => 1 + nbElementsDansNamed(result)
      case NamedLambda.application(function, argument) => 1 + nbElementsDansNamed(function) + nbElementsDansNamed(argument)
    }

  /**
   * Compte le nombre d'éléments dans une expression utilisant des indices de Bruijn
   * @param bruijnLambda L'expression à évaluer
   * @return Le nombre d'éléments dans l'expression
   */
  def nbElementsDansBruijn(bruijnLambda: BruijnLambda): Int =
    bruijnLambda match {
      case BruijnLambda.variableLiee(index) => 1
      case BruijnLambda.variableLibre(nom) => 1
      case BruijnLambda.abstraction(result) => 1 + nbElementsDansBruijn(result)
      case BruijnLambda.application(function, argument) => 1 + nbElementsDansBruijn(function) + nbElementsDansBruijn(argument)
    }


  /**
   * Réalise une étape d'évaluation stricte (c'est à dire une évaluation qui évalue en premier lieu les arguments des applications)
   * @param bruijnLambda L'expression à réduire par beta evaluation
   * @return L'expression après une étape d'évaluation stricte
   */
  def etapeEvaluationStricte(bruijnLambda: BruijnLambda): BruijnLambda =
    bruijnLambda match {
      case BruijnLambda.abstraction(result) => BruijnLambda.abstraction(etapeEvaluationStricte(result))
      case BruijnLambda.application(function, argument) =>
        if (estFormeNormale(function) && estFormeNormale(argument)) function match {
          case BruijnLambda.abstraction(_) => replaceAll(function, argument, 0)
          case _ => bruijnLambda
        }
        else if (!estFormeNormale(argument))  BruijnLambda.application(function, etapeEvaluationStricte(argument))
        else BruijnLambda.application(etapeEvaluationStricte(function), argument)
      case _ => bruijnLambda
    }


  /**
   * Réalise une réduction stricte totale d'une expression utilisant des variables nommées en passant par les indices de Bruijn
   * @param namedLambda L'expression à réduire
   * @return Une expression totalement réduite de l'expression donnée (le nom des variables liées changera)
   */
  def evaluationStricteTotale(namedLambda: NamedLambda): NamedLambda =
    bruijnToNamed(evaluationStricteTotale(namedToBruijn(namedLambda)))

  /**
   * Réalise une réduction stricte totale d'une expression utilisant les indices de Bruijn
   * @param bruijnLambda L'expression à réduire
   * @return Une expression en forme normale
   */
  @tailrec
  def evaluationStricteTotale(bruijnLambda: BruijnLambda): BruijnLambda =
    if (estFormeNormale(bruijnLambda)) bruijnLambda else evaluationStricteTotale(etapeEvaluationStricte(bruijnLambda))

  /**
   * Compte le nombre d'étapes pour réaliser une réduction stricte totale
   * @param bruijnLambda L'expression à évaluer
   * @return Le nombre de beta réductions réalisées lors d'une réduction stricte totale
   */
  def nbEtapesEvaluationStricte(bruijnLambda: BruijnLambda): Int =
    @tailrec
    def aux(bruijnLambda: BruijnLambda, nbEtapes: Int): Int =
      if (estFormeNormale(bruijnLambda)) nbEtapes else aux(etapeEvaluationStricte(bruijnLambda), nbEtapes+1)
    aux(bruijnLambda, 0)

  /**
   * Compte le nombre d'étapes pour réaliser une réduction paresseuse totale
   * @param bruijnLambda L'expression à évaluer
   * @return Le nombre de beta réductions réalisées lors d'une réduction paresseuse totale
   */
  def nbEtapesReductionParesseuseTotale(bruijnLambda: BruijnLambda): Int =
    @tailrec
    def aux(bruijnLambda: BruijnLambda, nbEtapes: Int): Int =
      if (estFormeNormale(bruijnLambda)) nbEtapes else aux(etapeReductionParesseuse(bruijnLambda), nbEtapes+1)
    aux(bruijnLambda, 0)

  @main def main(): Unit =
    val u = NamedLambda.variable("u")
    val v = NamedLambda.variable("v")
    val w = NamedLambda.variable("w")
    val x = NamedLambda.variable("x")
    val y = NamedLambda.variable("y")
    val z = NamedLambda.variable("z")

    val a_appli1 = NamedLambda.application(y, v)
    val a_abstract1 = NamedLambda.abstraction("v", a_appli1)
    val a_appli2 = NamedLambda.application(y, a_abstract1)
    val a_appli3 = NamedLambda.application(a_appli2, u)
    val a_abstract2 = NamedLambda.abstraction("z", a_appli3)
    val a_abstract3 = NamedLambda.abstraction("y", a_abstract2)
    val a_abstract4 = NamedLambda.abstraction("x", a_abstract3)
    val a_bruijnLambda = namedToBruijn(a_abstract4)
    println(namedToString(a_abstract4))
    println(bruijnToString(a_bruijnLambda))
    println(namedToString(bruijnToNamed(a_bruijnLambda)))

    println("\n-----------\n")

    val b_abstract1 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", y))
    val b_abstract2 = NamedLambda.abstraction("z", z)
    val b_appli1 = NamedLambda.application(b_abstract2, u)
    val b_appli2 = NamedLambda.application(b_abstract1, b_appli1)
    val b_appli_final = NamedLambda.application(b_appli2, v)
    val b_bruijnLambda = namedToBruijn(b_appli_final)

    println("L'expression avec des variables nommées              : " + namedToString(b_appli_final))
    println("L'expression avec des indices de Bruijn              : " + bruijnToString(b_bruijnLambda))
    println("L'expression après une étape d'évaluation paresseuse : " + bruijnToString(etapeReductionParesseuse(b_bruijnLambda)))
    println("L'expression après une étape d'évaluation stricte    : " + bruijnToString(etapeEvaluationStricte(b_bruijnLambda)))
    println("L'expression totalement réduite                      : " + bruijnToString(evaluationStricteTotale(b_bruijnLambda)))

}

