package org.specs2.control

import Eff._
  import Effects._

sealed trait Reader[I, X]

case class Get[I]() extends Reader[I, I]

object Reader {

  def ask[R, I](implicit member: Member[Reader[I, ?], R]): Eff[R, I] =
    impure(member.inject(Get[I]()), Arrs.singleton((i: I) => EffMonad[R].point(i)))

  def runReaderOnly[R, A](initial: A)(r: Eff[R, A])(implicit member: Member[Reader[A, ?], R]): A =
    r match {
      case Pure(run) => run()

      case Impure(union, continuation) =>
        member.project(union).map(_ => runReaderOnly(initial)(continuation.apply(initial))).getOrElse(initial)
    }

  def runReader[R <: Effects, A, B](initial: A)(r: Eff[Reader[A, ?] <:: R, B]): Eff[R, B] = {
    val readRest = new EffCont[Reader[A, ?], R, B] {
      def apply[X](r: Reader[A, X])(continuation: X => Eff[R, B]): Eff[R, B] = r match {
        case Get() => continuation(initial.asInstanceOf[X])
      }
    }

    relay1[R, Reader[A, ?], B, B]((b: B) => b)(readRest)(r)
  }

}
