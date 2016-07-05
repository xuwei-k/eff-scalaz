package org.atnos.eff.syntax

import scalaz._
import org.atnos.eff._

object disjunction extends disjunction

trait disjunction {

  implicit class DisjunctionEffectOps[R, A](e: Eff[R, A]) {

    def runDisjunction[E](implicit member: Member[(E \/ ?), R]): Eff[member.Out, E \/ A] =
      DisjunctionInterpretation.runDisjunction(e)(member.aux)

    def runEither[E, U](implicit member: Member[(E \/ ?), R]): Eff[member.Out, E Either A] =
      DisjunctionInterpretation.runEither(e)(member.aux)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
      DisjunctionInterpretation.catchLeft(e)(handle)(member)

    def localDisjunction[BR, U, C, B](getter: C => B)(implicit m1: Member.Aux[C \/ ?, R, U], m2: Member.Aux[B \/ ?, BR, U]): Eff[BR, A] =
      DisjunctionInterpretation.localDisjunction[R, BR, U, C, B, A](e, getter)
  }

}
