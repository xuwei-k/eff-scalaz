package org.specs2.control

import Eff._
import Effects._
import Member._

sealed trait Reader[I, X]

case class Get[I]() extends Reader[I, I]

object Reader {

  def ask[R, I](implicit member: Member[Reader[I, ?], R]): Eff[R, I] =
    impure(member.inject(Get[I]()), Arrs.singleton((i: I) => EffMonad[R].point(i)))

  def runReaderOnly[R, A](initial: A)(r: Eff[R, A])(implicit member: Member[Reader[A, ?], R]): A =
    r match {
      case p@Pure(_) => p.value

      case Impure(union, continuation) =>
        member.project(union).map(_ => runReaderOnly(initial)(continuation.apply(initial))).getOrElse(initial)
    }

  def runReader[R <: Effects, A, B](initial: A)(r: Eff[Reader[A, ?] <:: R, B]): Eff[R, B] = {
    def loop(eff: Eff[Reader[A, ?] <:: R, B]): Eff[R, B] = {
      if (eff.isInstanceOf[Pure[Reader[A, ?] <:: R, B]])
         EffMonad[R].point(eff.asInstanceOf[Pure[Reader[A, ?] <:: R, B]].value)
      else {
        val i = eff.asInstanceOf[Impure[Reader[A, ?] <:: R, B]]
        val d = decompose[Reader[A, ?], R, B](i.union.asInstanceOf[Union[Reader[A, ?] <:: R, B]])
        if (d.toOption.isDefined)
          loop(i.continuation(initial))
        else {
          val u = d.toEither.left.toOption.get
          Impure[R, B](u.asInstanceOf[Union[R, Any]], Arrs.singleton(x => loop(i.continuation(x))))
        }
      }
    }

    loop(r)
  }
}
