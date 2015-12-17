package org.specs2.control

import scalaz._
import Eff._
import Member._
import Effects._

sealed trait Checked[E, A]

case class CheckedKo[E, A](e: E) extends Checked[E, A]
case class CheckedOk[E, A](a: A) extends Checked[E, A]

object Checked {

  def ko[R, E, A](e: E)(implicit member: Member[Checked[E, ?], R]): Eff[R, A] =
    impure(member.inject(CheckedKo[E, A](e)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def ok[R, E, A](a: A)(implicit member: Member[Checked[E, ?], R]): Eff[R, A] =
    impure(member.inject(CheckedOk[E, A](a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def runChecked[R <: Effects, E, A](r: Eff[Checked[E, ?] <:: R, A]): Eff[R, Either[E, A]] = {
    val runImpure = new EffCont[Checked[E, ?], R, Either[E, A]] {
      def apply[X](r: Checked[E, X])(continuation: X => Eff[R, Either[E, A]]): Eff[R, Either[E, A]] = r match {
        case CheckedKo(e) => pure(Left[E, A](e): Either[E, A])
        case CheckedOk(a) => continuation(a)
      }
    }

    relay1[R, Checked[E, ?], A, Either[E, A]]((a: A) => Right(a): Either[E, A])(runImpure)(r)
  }
}

object CheckedErrorEff {
  type Error = Throwable \/ String

  type CheckedError[A] = Checked[Error, A]

  def fail[R, A](message: String)(implicit m: CheckedError <= R): Eff[R, A] =
    Checked.ko[R, Error, A](\/-(message))

  def exception[R, A](t: Throwable)(implicit m: CheckedError <= R): Eff[R, A] =
    Checked.ko[R, Error, A](-\/(t))
}
