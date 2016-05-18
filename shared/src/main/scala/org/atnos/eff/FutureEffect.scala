package org.atnos.eff

import scala.util.control.NonFatal
import scalaz._, scalaz._
import Eff._
import Interpret._

import scala.concurrent._
import duration._

/**
 * Effect for Future computations
 */
trait FutureEffect extends
  FutureCreation with
  FutureInterpretation

object FutureEffect extends FutureEffect

trait FutureCreation {
  def sync[R, A](a: A)(implicit m: Member[Future, R]): Eff[R, A] =
    pure(a)

  def async[R, A](a: =>A)(implicit m: Member[Future, R], ec: ExecutionContext): Eff[R, A] =
    send(Future(a))
}

trait FutureInterpretation {

  def awaitFuture[R <: Effects, U <: Effects, A](r: Eff[R, A])(atMost: FiniteDuration)
      (implicit m: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable \/ A] = {
    val recurse = new Recurse[Future, U, Throwable \/ A] {
      def apply[X](m: Future[X]) =
        try { -\/(Await.result(m, atMost)) }
        catch { case NonFatal(t) => \/-(Eff.pure(-\/(t))) }
    }

    interpret1((a: A) => \/-(a): Throwable \/ A)(recurse)(r)
  }

}

object FutureInterpretation extends FutureInterpretation

