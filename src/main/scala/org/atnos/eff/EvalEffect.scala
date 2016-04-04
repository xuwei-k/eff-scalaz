package org.atnos.eff

import scala.util.control.NonFatal
import scalaz._
import Eff._
import Interpret._

/**
 * Effect for delayed computations
 *
 * uses scalaz.Need as a supporting data structure
 *
 */
object EvalEffect {

   type Eval[A] = scalaz.Need[A]

  def now[R, A](a: A)(implicit m: Member[Eval, R]): Eff[R, A] =
    pure(a)

  def delay[R, A](a: =>A)(implicit m: Member[Eval, R]): Eff[R, A] =
    send(Need(a))

  def runEval[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, A] = {
    val recurse = new Recurse[Eval, U, A] {
      def apply[X](m: Eval[X]) = -\/(m.value)
    }

    interpret1((a: A) => a)(recurse)(r)
  }

  def attemptEval[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, Throwable \/ A] = {
    val recurse = new Recurse[Eval, U, Throwable \/ A] {
      def apply[X](m: Eval[X]) =
        try { -\/(m.value) }
        catch { case NonFatal(t) => \/-(Eff.pure(-\/(t))) }
    }

    interpret1((a: A) => \/-(a): Throwable \/ A)(recurse)(r)
  }

}

