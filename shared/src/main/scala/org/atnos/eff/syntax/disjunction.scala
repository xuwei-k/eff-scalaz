package org.atnos.eff.syntax

import scalaz._
import org.atnos.eff._

object disjunction extends disjunction

trait disjunction {

  implicit class DisjunctionEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runDisjunction[E](implicit member: Member[(E \/ ?), R]): Eff[member.Out, E \/ A] =
      DisjunctionInterpretation.runDisjunction(e)(member.aux)

    def runEither[E, U <: Effects](implicit member: Member[(E \/ ?), R]): Eff[member.Out, E Either A] =
      DisjunctionInterpretation.runEither(e)(member.aux)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
      DisjunctionInterpretation.catchLeft(e)(handle)(member)
  }

}
