package org.specs2
package control

import Eff._
import Effects._
import org.specs2.control.Member._
import scala.util.control.NonFatal
import scalaz._, Scalaz._
import scalaz.effect.IO


sealed trait Eval[A] {
  def value: A
}

case class Evaluate[A](v: () => A) extends Eval[A] {
  def value: A =
    v()
}

object Eval {

  def now[R, A](a: A)(implicit m: Member[Eval, R]): Eff[R, A] =
    pure(a)

  def delay[R, A](a: =>A)(implicit m: Member[Eval, R]): Eff[R, A] =
    impure(m.inject(Evaluate[A](() => a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def evalIO[R, A](a: IO[A])(implicit m: Member[Eval, R]): Eff[R, A] =
    delay(a.unsafePerformIO)

  def runEval[R <: Effects, A](r: Eff[Eval[?] <:: R, A]): Eff[R, A] = {
    def loop(eff: Eff[Eval <:: R, A]): Eff[R, A] = {
      if (eff.isInstanceOf[Pure[Eval <:: R, A]])
         EffMonad[R].point(eff.asInstanceOf[Pure[Eval <:: R, A]].value)
      else {
        val i = eff.asInstanceOf[Impure[Eval <:: R, A]]
        val d = decompose[Eval, R, A](i.union.asInstanceOf[Union[Eval <:: R, A]])
        if (d.toOption.isDefined)
          loop(i.continuation(d.toOption.get.asInstanceOf[Eval[A]].value))
        else {
          val u = d.toEither.left.toOption.get
          Impure[R, A](u.asInstanceOf[Union[R, Any]], Arrs.singleton(x => loop(i.continuation(x))))
        }
      }
    }

    loop(r)
  }

  def attemptEval[R <: Effects, A](r: Eff[Eval[?] <:: R, A]): Eff[R, Throwable \/ A] = {
    val bind = new EffBind[Eval, R, Throwable \/ A] {
      def apply[X](r: Eval[X])(continuation: X => Eff[R, Throwable \/ A]): Eff[R, Throwable \/ A] =
        try { continuation(r.value) }
        catch { case NonFatal(t) => Eff.pure(-\/(t)) }
    }

    interpret1[R, Eval, A, Throwable \/ A]((a: A) => \/-(a))(bind)(r)
  }

  implicit class AndFinally[R, A](action: Eff[R, A]) {
    def andFinally(last: Eff[R, Unit])(implicit m: Eval <= R): Eff[R, A] =
      Eval.andFinally(action, last)

    def orElse(action2: Eff[R, A])(implicit m: Eval <= R): Eff[R, A] =
      Eval.orElse(action, action2)
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
            Eval.delay { try e1.value.asInstanceOf[A] finally { e2.value; () } }

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
        try Eval.now(p1.value)
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

