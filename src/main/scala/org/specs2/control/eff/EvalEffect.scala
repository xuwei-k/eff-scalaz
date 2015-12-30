package org.specs2.control.eff

import scala.util.control.NonFatal
import scalaz._
import scalaz.effect.IO
import Eff._
import Effects._
import Interpret._

/**
 * Effect for delayed computations
 *
 * uses scalaz.Name as a data structure
 *
 */
object EvalEffect {

   type Eval[A] = Name[A]

  def now[R, A](a: A)(implicit m: Member[Eval, R]): Eff[R, A] =
    pure(a)

  def delay[R, A](a: =>A)(implicit m: Member[Eval, R]): Eff[R, A] =
    send(Name(a))

  def evalIO[R, A](a: IO[A])(implicit m: Member[Eval, R]): Eff[R, A] =
    delay(a.unsafePerformIO)

  def runEval[R <: Effects, A](r: Eff[Eval[?] |: R, A]): Eff[R, A] = {
    val recurse = new Recurse[Eval, R, A] {
      def apply[X](m: Eval[X]) = -\/(m.value)
    }

    interpret1((a: A) => a)(recurse)(r)
  }

  def attemptEval[R <: Effects, A](r: Eff[Eval[?] |: R, A]): Eff[R, Throwable \/ A] = {
    val recurse = new Recurse[Eval, R, Throwable \/ A] {
      def apply[X](m: Eval[X]) =
        try { -\/(m.value) }
        catch { case NonFatal(t) => \/-(Eff.pure(-\/(t))) }
    }

    interpret1((a: A) => \/-(a): Throwable \/ A)(recurse)(r)
  }

}

