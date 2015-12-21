package org.specs2.control

import Eff._
import Effects._
import scalaz._

/**
 * Effect for depending on a value of type I
 */
object ReaderEffect {

  def ask[R, I](implicit member: Member[Reader[I, ?], R]): Eff[R, I] =
    impure(member.inject(Reader(identity _)), Arrs.singleton((i: I) => EffMonad[R].point(i)))

  def runReader[R <: Effects, A, B](env: A)(r: Eff[Reader[A, ?] <:: R, B]): Eff[R, B] = {
    val recurse = new Recurse[Reader[A, ?], R, B] {
      def apply[X](m: Reader[A, X]) = -\/(env.asInstanceOf[X])
    }

    interpret1[R, Reader[A, ?], B, B]((b: B) => b)(recurse)(r)
  }
}
