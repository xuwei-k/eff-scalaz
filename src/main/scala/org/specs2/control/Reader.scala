package org.specs2.control

import scalaz._, Scalaz._
import Eff._
import Member._
import Reader._
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

  def runReader[R <: Effects, A](initial: A)(r: Eff[Reader[A, ?] <:: R, A]): Eff[R, A] = {
    val readOne = (a: A) => EffMonad[R].point(a)

    val readRest = new EffCont[Reader[A, ?], R, A] {
      def apply[X] = (r: Reader[A, X]) => (continuation: X => Eff[R, A]) => r match {
        case Get() => continuation(initial.asInstanceOf[X])
      }
    }

    relay[R, Reader[A, ?], A, A](readOne, readRest)(r)
  }

  type ReaderStack[A, E <: Effects] = Reader[A, ?] <:: E

  implicit def ReaderMember[A, E <: Effects]: Member[Reader[A, ?], ReaderStack[A, E]] =
    Member.EffectMember[Reader[A, ?], E]

}
