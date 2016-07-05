package org.atnos.eff.syntax

import scalaz._
import org.atnos.eff._

object choose extends choose

trait choose {

  implicit class ChooseEffectOps[R, A](e: Eff[R, A]) {

    def runChoose[F[_] : MonadPlus](implicit member: Member[Choose, R]): Eff[member.Out, F[A]] =
      ChooseInterpretation.runChoose(e)(MonadPlus[F], member.aux)

  }

}
