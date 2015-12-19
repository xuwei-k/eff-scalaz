package org.specs2.control

import Eff._
import Effects._
import scalaz._

sealed trait Reader[I, X]

case class Get[I]() extends Reader[I, I]

object Reader {

  def ask[R, I](implicit member: Member[Reader[I, ?], R]): Eff[R, I] =
    impure(member.inject(Get[I]()), Arrs.singleton((i: I) => EffMonad[R].point(i)))

  def runReader[R <: Effects, A, B](initial: A)(r: Eff[Reader[A, ?] <:: R, B]): Eff[R, B] = {
    val bind = new Binder[Reader[A, ?], R, B] {
      def apply[X](m: Reader[A, X]) = -\/(initial.asInstanceOf[X])
    }

    interpretLoop1[R, Reader[A, ?], B, B]((b: B) => b)(bind)(r)
  }
}
