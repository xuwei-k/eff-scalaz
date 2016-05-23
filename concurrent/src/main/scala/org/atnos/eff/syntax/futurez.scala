package org.atnos.eff.syntax

import org.atnos.eff._
import scala.concurrent.duration._
import scalaz.concurrent._
import scalaz.\/

object futurez extends futurez

trait futurez {

  implicit class FuturezEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def attemptFuture[U <: Effects](atMost: Duration)
      (implicit member: Member.Aux[Future, R, U]): Eff[U, Throwable \/ A] =
      FuturezInterpretation.attemptFuture(e)(atMost)

  }

}
