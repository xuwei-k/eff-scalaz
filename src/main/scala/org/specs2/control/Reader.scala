package org.specs2.control

import scalaz.{Coproduct=>_,Inject=>_,:+: => _,_}, Scalaz._
import Eff._
import Effects._
import Members._
import shapeless._, shapeless.ops.coproduct._

sealed trait Reader[I, X]

case class Get[I]() extends Reader[I, I]

object Reader {

  def ask[R <: Coproduct, I](implicit member: Members[Reader[I, ?], R], inject: Inject[R, Reader[I, I]]): Eff[R, I] =
    impure(member.inject(Get[I]()), Arrs.singleton((i: I) => EffMonad[R].point(i)))

  def runReaderOnly[R <: Coproduct, A](initial: A)(r: Eff[R, A])(implicit member: Members[Reader[A, ?], R], select: Selector[R, Reader[A, A]]): A =
    r match {
      case Pure(run) => run()

      case Impure(union, continuation) =>
        member.project(union.asInstanceOf[Unions[R, A]]).map(_ => runReaderOnly(initial)(continuation.apply(initial))).getOrElse(initial)
    }

  def runReader[R <: Coproduct, A, B](r: Eff[Reader[A, B] :+: R, B])(initial: A): Eff[R, B] = {
    val readOne = (b: B) => EffMonad[R].point(b)

    val readRest = new EffCont[Reader[A, ?], R, B] {
      def apply[X] = (r: Reader[A, X]) => (continuation: X => Eff[R, B]) => r match {
        case Get() => continuation(initial.asInstanceOf[X])
      }
    }

    relay[R, Reader[A, ?], B, B](readOne, readRest)(r)
  }

  type ReaderStack[A, E <: Effects] = Reader[A, ?] <:: E

  implicit def ReaderMember[A, E <: Effects]: Member[Reader[A, ?], ReaderStack[A, E]] =
    Member.EffectMember[Reader[A, ?], E]

}
