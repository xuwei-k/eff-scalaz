package org.specs2
package control

import Eff._
import Effects._
import org.specs2.control.Member._
import scala.util.control.NonFatal
import scalaz._, Scalaz._
import scalaz.effect.IO

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
    impure(m.inject(Name(a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def evalIO[R, A](a: IO[A])(implicit m: Member[Eval, R]): Eff[R, A] =
    delay(a.unsafePerformIO)

  def runEval[R <: Effects, A](r: Eff[Eval[?] <:: R, A]): Eff[R, A] = {
    val recurse = new Recurse[Eval, R, A] {
      def apply[X](m: Eval[X]) = -\/(m.value)
    }

    interpret1((a: A) => a)(recurse)(r)
  }

  def attemptEval[R <: Effects, A](r: Eff[Eval[?] <:: R, A]): Eff[R, Throwable \/ A] = {
    val recurse = new Recurse[Eval, R, Throwable \/ A] {
      def apply[X](m: Eval[X]) =
        try { -\/(m.value) }
        catch { case NonFatal(t) => \/-(Eff.pure(-\/(t))) }
    }

    interpret1((a: A) => \/-(a): Throwable \/ A)(recurse)(r)
  }

  implicit class AndFinally[R, A](action: Eff[R, A]) {
    def andFinally(last: Eff[R, Unit])(implicit m: Eval <= R): Eff[R, A] =
      EvalEffect.andFinally(action, last)

    def orElse(action2: Eff[R, A])(implicit m: Eval <= R): Eff[R, A] =
      EvalEffect.orElse(action, action2)
  }

  /**
   * evaluate 2 actions possibly having eval effects
   *
   * The second action must be executed whether the first is successful or not
   */
  def andFinally[R, A](action: Eff[R, A], last: Eff[R, Unit])(implicit m: Eval <= R): Eff[R, A] =
    (action, last) match {
      case (_, Pure(l))                     => action
      case (Pure(_), Impure(u, c))          => action >>= ((a: A) => last.as(a))
      case (Impure(u1, c1), Impure(u2, c2)) =>
        (m.project(u1), m.project(u2)) match {
          case (Some(e1), Some(e2)) =>
            EvalEffect.delay { try e1.value.asInstanceOf[A] finally { e2.value; () } }

          case _ => action
        }
    }

  /**
   * evaluate 2 actions possibly having eval effects
   *
   * The second action must be executed if the first one is not successful
   */
  def orElse[R, A](action1: Eff[R, A], action2: Eff[R, A])(implicit m: Eval <= R): Eff[R, A] =
    (action1, action2) match {
      case (p1@Pure(_), _) =>
        try EvalEffect.now(p1.value)
        catch { case _ : Throwable => action2 }

      case (Impure(u1, c1), _) =>
        m.project(u1) match {
          case Some(e1) =>
            try c1(e1.value.asInstanceOf[A])
            catch { case _: Throwable => action2 }

          case None => action1
        }
    }
}

