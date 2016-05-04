package org.atnos.eff

import scalaz._, Scalaz._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2.{ScalaCheck, Specification}

class ValidateEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the validate effect                     $validateOk
 run the validate effect with nothing        $validateKo

 run is stack safe with Validate  $stacksafeRun

"""

  def validateOk = {
    type S = ValidateString |: NoEffect

    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.correct[S, String, Int](2)
        a <- EffMonad[S].point(3)
      } yield a

    validate.runNel.run ==== \/-(3)
  }

  def validateKo = {
    type S = ValidateString |: NoEffect

    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.wrong[S, String]("error!")
        a <- EffMonad[S].point(3)
      } yield a

    validate.runNel.run ==== -\/(NonEmptyList("error!"))
  }

  type ValidateString[A] = Validate[String, A]

  def stacksafeRun = {
    type E = ValidateString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => ValidateEffect.wrong[E, String](i.toString))

    action.runNel.run.leftMap(_.toList) ==== \/.left(list.map(_.toString))
  }

}

