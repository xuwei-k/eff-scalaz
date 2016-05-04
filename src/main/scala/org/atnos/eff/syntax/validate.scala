package org.atnos.eff.syntax

import scalaz.{NonEmptyList, ValidationNel, \/}
import org.atnos.eff._
import scalaz.Semigroup

object validate extends validate

trait validate {

  implicit class ValidateEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runNel[E, U <: Effects](implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, NonEmptyList[E] \/ A] =
      ValidateInterpretation.runNel(e)

    def runMap[E, U <: Effects, L : Semigroup](map: E => L)(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, L \/ A] =
      ValidateInterpretation.runMap(e)(map)

    def runValidatedNel[E, U <: Effects](implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, ValidationNel[E, A]] =
      ValidateInterpretation.runValidationNel(e)
  }

}
