package org.specs2.control

import scalaz._, Scalaz._
import Eff._
import Member._
import MemberNat._
import Optional._
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
    val runPure = (a: A) => EffMonad[R].point(Right[E, A](a): Either[E, A])

    val runImpure = new EffCont[Checked[E, ?], R, Either[E, A]] {
      def apply[X](r: Checked[E, X])(continuation: X => Eff[R, Either[E, A]]): Eff[R, Either[E, A]] = r match {
        case CheckedKo(e) => pure(Left[E, A](e): Either[E, A])
        case CheckedOk(a) => continuation(a)
      }
    }

    relay[R, Checked[E, ?], A, Either[E, A]](runPure, runImpure)(r)
  }
}

