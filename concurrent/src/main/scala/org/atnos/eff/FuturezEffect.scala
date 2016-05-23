package org.atnos.eff

import scala.util.control.NonFatal
import Eff._
import Interpret._
import scalaz._
import scalaz.concurrent._
import scala.concurrent.duration._

/**
 * Effect for scalaz.Future computations
 */
trait FuturezEffect extends
  FuturezCreation with
  FuturezInterpretation

object FuturezEffect extends FuturezEffect

trait FuturezCreation {
  def immediate[R, A](a: A)(implicit m: Member[Future, R]): Eff[R, A] =
    pure(a)

  def delayed[R, A](a: =>A)(implicit m: Member[Future, R]): Eff[R, A] =
    send(Future.delay(a))
}

trait FuturezInterpretation {

  def attemptFuture[R <: Effects, U <: Effects, A](r: Eff[R, A])(atMost: Duration)
      (implicit m: Member.Aux[Future, R, U]): Eff[U, Throwable \/ A] = {
    val recurse = new Recurse[Future, U, Throwable \/ A] {
      def apply[X](m: Future[X]) =
        try { m.unsafePerformSyncAttemptFor(atMost).swap.map(e => Eff.pure(\/.left(e))) }
        catch { case NonFatal(t) => \/-(Eff.pure(-\/(t))) }
    }

    interpret1((a: A) => \/-(a): Throwable \/ A)(recurse)(r)
  }

}

object FuturezInterpretation extends FuturezInterpretation

