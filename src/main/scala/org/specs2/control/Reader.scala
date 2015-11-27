package org.specs2.control

import scalaz._, Scalaz._
import Eff._
import Member._
import Reader._
import Effects._

trait Reader[I, X]

case class Get[I]() extends Reader[I, I]

object Reader {
  def ask[I, R](implicit member: Member[Reader[I, ?], R]): Eff[R, I] =
    new Impure[R, I] {
      type X = I
      val union = member.inject(Get[I]())
      val continuation = (x: X) => EffMonad[R].point(x)
    }

  def runReader[R, A](initial: A)(r: Eff[R, A])(implicit member: Member[Reader[A, ?], R]): A =
    r match {
      case Pure(run) => run()

      case impure: Impure[R, A] =>
        member.project(impure.union).map(rx => runReader(initial)(impure.continuation(initial.asInstanceOf[impure.X]))).getOrElse(initial)
    }


  type EffectStack[A, E <: Effects] = Reader[A, ?] <:: E

  implicit def ReaderMember[A, E <: Effects]: Member[Reader[A, ?], EffectStack[A, E]] =
    Member.EffectMember[Reader[A, ?], E]


}
