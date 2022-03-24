import App.*
import org.scalatest.flatspec.AnyFlatSpec

class AppTests extends AnyFlatSpec {

  val u: NamedLambda = NamedLambda.variable("u")
  val v: NamedLambda = NamedLambda.variable("v")
  val w: NamedLambda = NamedLambda.variable("w")
  val x: NamedLambda = NamedLambda.variable("x")
  val y: NamedLambda = NamedLambda.variable("y")
  val z: NamedLambda = NamedLambda.variable("z")

  behavior of "Transitions entre les deux types d'écriture "

  it should "être identiques au niveau des indices de Bruijn" in {
    val c_appli1 = NamedLambda.application(u, x)
    val c_abstr1 = NamedLambda.abstraction("u", c_appli1)
    val c_appli2 = NamedLambda.application(z,x)
    val c_appli3 = NamedLambda.application(c_appli2, c_abstr1)
    val c_abstr2 = NamedLambda.abstraction("y", c_appli3)
    val c_abstr3 = NamedLambda.abstraction("x", c_abstr2)
    val c_appli4 = NamedLambda.application(w, x)
    val c_abstr4 = NamedLambda.abstraction("x", c_appli4)
    val c_final = NamedLambda.application(c_abstr3, c_abstr4)
    val bruijnLambda = namedToBruijn(c_final)

    assert(bruijnLambda.equals(namedToBruijn(bruijnToNamed(bruijnLambda))))
  }

  it should "avoir autant de termes entre le Nommé et le Bruijin" in {
    val c_appli1 = NamedLambda.application(u, x)
    val c_abstr1 = NamedLambda.abstraction("u", c_appli1)
    val c_appli2 = NamedLambda.application(z,x)
    val c_appli3 = NamedLambda.application(c_appli2, c_abstr1)
    val c_abstr2 = NamedLambda.abstraction("y", c_appli3)
    val c_abstr3 = NamedLambda.abstraction("x", c_abstr2)
    val c_appli4 = NamedLambda.application(w, x)
    val c_abstr4 = NamedLambda.abstraction("x", c_appli4)
    val c_final = NamedLambda.application(c_abstr3, c_abstr4)
    val bruijnLambda = namedToBruijn(c_final)

    assert(nbElementsDansNamed(c_final) == nbElementsDansBruijn(bruijnLambda))
  }

  behavior of "Réduction paresseuse"

  it should "Réduire correctement une expression utilisant des indices de Bruijn" in {
    val abstract1 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", y))
    val abstract2 = NamedLambda.abstraction("z", z)
    val appli1 = NamedLambda.application(abstract2, u)
    val appli2 = NamedLambda.application(abstract1, appli1)
    val appli_final = NamedLambda.application(appli2, v)
    val bruijnLambda = namedToBruijn(appli_final)

    assert(reductionParesseuseTotale(bruijnLambda).equals(BruijnLambda.variableLibre("v")))
  }

  it should "Réduire correctement une expression utilisant des variables nommées" in {
    val abstract1 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", y))
    val abstract2 = NamedLambda.abstraction("z", z)
    val appli1 = NamedLambda.application(abstract2, u)
    val appli2 = NamedLambda.application(abstract1, appli1)
    val appli_final = NamedLambda.application(appli2, v)

    assert(reductionParesseuseTotale(appli_final).equals(NamedLambda.variable("v")))
  }

  it should "Ne rien faire lorsqu'une expression est en forme normale" in {
    val expression = NamedLambda.abstraction("x", NamedLambda.abstraction("y",y))

    assert(nbEtapesReductionParesseuseTotale(namedToBruijn(expression)) == 0)
  }

  it should "Faire moins d'étapes que d'abstractions appliquées" in {
    val abstract1 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", y))
    val abstract2 = NamedLambda.abstraction("z", z)
    val appli1 = NamedLambda.application(abstract2, u)
    val appli2 = NamedLambda.application(abstract1, appli1)
    val appli_final = NamedLambda.application(appli2, v)
    val bruijnLambda = namedToBruijn(appli_final)

    assert(nbEtapesReductionParesseuseTotale(bruijnLambda) < nbLambdasDansTerme(bruijnLambda))
  }

  it should "Faire moins d'étapes que l'évaluation stricte" in {
    val abstract1 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", y))
    val abstract2 = NamedLambda.abstraction("z", z)
    val appli1 = NamedLambda.application(abstract2, u)
    val appli2 = NamedLambda.application(abstract1, appli1)
    val appli_final = NamedLambda.application(appli2, v)
    val bruijnLambda = namedToBruijn(appli_final)

    assert(nbEtapesReductionParesseuseTotale(bruijnLambda) < nbEtapesEvaluationStricte(bruijnLambda))
  }

  behavior of "Evaluation stricte"

  it should "Réduire correctement une expression utilisant des indices de Bruijn" in {
    val abstract1 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", y))
    val abstract2 = NamedLambda.abstraction("z", z)
    val appli1 = NamedLambda.application(abstract2, u)
    val appli2 = NamedLambda.application(abstract1, appli1)
    val appli_final = NamedLambda.application(appli2, v)
    val bruijnLambda = namedToBruijn(appli_final)

    assert(evaluationStricteTotale(bruijnLambda).equals(BruijnLambda.variableLibre("v")))
  }

  it should "Réduire correctement une expression utilisant des variables nommées" in {
    val abstract1 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", y))
    val abstract2 = NamedLambda.abstraction("z", z)
    val appli1 = NamedLambda.application(abstract2, u)
    val appli2 = NamedLambda.application(abstract1, appli1)
    val appli_final = NamedLambda.application(appli2, v)

    assert(evaluationStricteTotale(appli_final).equals(NamedLambda.variable("v")))
  }

  it should "Ne rien faire lorsqu'une expression est en forme normale" in {
    val expression = NamedLambda.abstraction("x", NamedLambda.abstraction("y",y))

    assert(nbEtapesReductionParesseuseTotale(namedToBruijn(expression)) == 0)
  }

  it should "Faire autant d'étapes que d'abstractions appliquées" in {
    val abstract1 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", y))
    val abstract2 = NamedLambda.abstraction("z", z)
    val appli1 = NamedLambda.application(abstract2, u)
    val appli2 = NamedLambda.application(abstract1, appli1)
    val appli_final = NamedLambda.application(appli2, v)
    val bruijnLambda = namedToBruijn(appli_final)

    assert(nbEtapesEvaluationStricte(bruijnLambda) == nbLambdasDansTerme(bruijnLambda))
  }

  it should "Faire plus d'étapes que la réduction paresseuse" in {
    val abstract1 = NamedLambda.abstraction("x",NamedLambda.abstraction("y", y))
    val abstract2 = NamedLambda.abstraction("z", z)
    val appli1 = NamedLambda.application(abstract2, u)
    val appli2 = NamedLambda.application(abstract1, appli1)
    val appli_final = NamedLambda.application(appli2, v)
    val bruijnLambda = namedToBruijn(appli_final)

    assert(nbEtapesEvaluationStricte(bruijnLambda) > nbEtapesReductionParesseuseTotale(bruijnLambda))
  }


}