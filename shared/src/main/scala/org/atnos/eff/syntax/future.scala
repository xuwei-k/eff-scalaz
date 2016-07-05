package org.atnos.eff.syntax

import org.atnos.eff._
import scala.concurrent._, duration._
import scalaz._

object future extends future

trait future {

  implicit class FutureEffectOps[R, A](e: Eff[R, A]) {

    def awaitFuture[U](atMost: FiniteDuration)
      (implicit member: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable \/ A] =
      FutureInterpretation.awaitFuture(e)(atMost)

  }

}
