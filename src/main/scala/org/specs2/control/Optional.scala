package org.specs2.control

import Eff._
import Effects._
import scalaz.{-\/, \/-}

/**
 * Effect for optional computations
 */
object Optional {

  sealed trait Optional[A]

  case class Nothing[A]() extends Optional[A]
  case class Something[A](a: A) extends Optional[A]

  def nothing[R, A](implicit member: Member[Optional[?], R]): Eff[R, A] =
    impure(member.inject(Nothing[A]()), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def something[R, A](a: A)(implicit member: Member[Optional[?], R]): Eff[R, A] =
    impure(member.inject(Something[A](a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def runOptional[R <: Effects, A](r: Eff[Optional[?] <:: R, A]): Eff[R, Option[A]] = {
    val recurse = new Recurse[Optional, R, Option[A]] {
      def apply[X](m: Optional[X]) =
        m match {
          case Nothing()    => \/-(EffMonad[R].point(None))
          case Something(x) => -\/(x)
        }
    }

    interpret1[R, Optional, A, Option[A]]((a: A) => Option(a))(recurse)(r)
  }
}

