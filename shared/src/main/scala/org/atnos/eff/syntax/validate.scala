package org.atnos.eff.syntax

import scalaz._
import org.atnos.eff._


object validate extends validate

trait validate {

  implicit class ValidationEffectOps[R, A](e: Eff[R, A]) {

    def runNel[E](implicit m: Member[Validate[E, ?], R]): Eff[m.Out, NonEmptyList[E] \/ A] =
      ValidateInterpretation.runNel(e)(m.aux)

    def runMap[E, L : Semigroup](map: E => L)(implicit m: Member[Validate[E, ?], R]): Eff[m.Out, L \/ A] =
      ValidateInterpretation.runMap(e)(map)(Semigroup[L], m.aux)

    def runValidationNel[E](implicit m: Member[Validate[E, ?], R]): Eff[m.Out, ValidationNel[E, A]] =
      ValidateInterpretation.runValidationNel(e)(m.aux)

    def catchWrong[E](handle: E => Eff[R, A])(implicit m: Member[Validate[E, ?], R]): Eff[R, A] =
      ValidateInterpretation.catchWrong(e)(handle)

  }

}
