package org.specs2.control

import scalaz._, Scalaz._
import Eff._
import Member._
import Effects._
import Checked._

/**
 * Effect for computation which can fail
 */
object Checked {

  sealed trait Checked[E, A]

  case class CheckedKo[E, A](e: E) extends Checked[E, A]
  case class CheckedOk[E, A](a: A) extends Checked[E, A]

  def ko[R, E, A](e: E)(implicit member: Member[Checked[E, ?], R]): Eff[R, A] =
    impure(member.inject(CheckedKo[E, A](e)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def ok[R, E, A](a: A)(implicit member: Member[Checked[E, ?], R]): Eff[R, A] =
    impure(member.inject(CheckedOk[E, A](a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def runChecked[R <: Effects, E, A](r: Eff[Checked[E, ?] <:: R, A]): Eff[R, E \/ A] = {
    val bind = new Binder[Checked[E, ?], R, E \/ A] {
      def apply[X](m: Checked[E, X]) =
        m match {
          case CheckedKo(e) => \/-(EffMonad[R].point(-\/(e): E \/ A))
          case CheckedOk(a) => -\/(a)
        }
    }

    interpretLoop1[R, Checked[E, ?], A, E \/ A]((a: A) => \/-(a))(bind)(r)
  }

  def runCheckedEither[R <: Effects, E, A](r: Eff[Checked[E, ?] <:: R, A]): Eff[R, Either[E, A]] =
    runChecked(r).map(_.fold(Left.apply, Right.apply))

}

/**
 * Effect for computation which can fail and return a Throwable
 */
object CheckedErrorEff {
  type Error = Throwable \/ String

  type CheckedError[A] = Checked[Error, A]

  def fail[R, A](message: String)(implicit m: CheckedError <= R): Eff[R, A] =
    Checked.ko[R, Error, A](\/-(message))

  def exception[R, A](t: Throwable)(implicit m: CheckedError <= R): Eff[R, A] =
    Checked.ko[R, Error, A](-\/(t))
}
