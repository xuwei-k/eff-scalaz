package org.specs2.control


import Eff._
import Effects._

sealed trait Optional[A]

case class Nothing[A]() extends Optional[A]
case class Something[A](a: A) extends Optional[A]

object Optional {

  def nothing[R, A](implicit member: Member[Optional[?], R]): Eff[R, A] =
    impure(member.inject(Nothing[A]()), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def something[R, A](a: A)(implicit member: Member[Optional[?], R]): Eff[R, A] =
    impure(member.inject(Something[A](a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def runOptional[R <: Effects, A](r: Eff[Optional[?] <:: R, A]): Eff[R, Option[A]] = {
    val runImpure = new EffCont[Optional, R, Option[A]] {
      def apply[X](r: Optional[X])(continuation: X => Eff[R, Option[A]]): Eff[R, Option[A]] = r match {
        case Nothing()   => pure(None)
        case Something(a) => continuation(a)
      }
    }

    relay1[R, Optional, A, Option[A]]((a: A) => Option(a))(runImpure)(r)
  }
}

