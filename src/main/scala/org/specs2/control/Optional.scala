package org.specs2.control

import Eff._
import Effects._
import Member._

sealed trait Optional[A]

case class Nothing[A]() extends Optional[A]
case class Something[A](a: A) extends Optional[A]

object Optional {

  def nothing[R, A](implicit member: Member[Optional[?], R]): Eff[R, A] =
    impure(member.inject(Nothing[A]()), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def something[R, A](a: A)(implicit member: Member[Optional[?], R]): Eff[R, A] =
    impure(member.inject(Something[A](a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def runOptional[R <: Effects, A](r: Eff[Optional[?] <:: R, A]): Eff[R, Option[A]] = {
    def loop(eff: Eff[Optional <:: R, A]): Eff[R, Option[A]] = {
      if (eff.isInstanceOf[Pure[Optional <:: R, A]])
         EffMonad[R].point(Option(eff.asInstanceOf[Pure[Optional <:: R, A]].value))
      else {
        val i = eff.asInstanceOf[Impure[Optional <:: R, A]]
        val d = decompose[Optional, R, A](i.union.asInstanceOf[Union[Optional <:: R, A]])
        if (d.toOption.isDefined)
          d.toOption.get match {
            case Nothing() => Eff.pure(None)
            case Something(a) => loop(i.continuation(a))
          }

        else {
          val u = d.toEither.left.toOption.get
          Impure[R, Option[A]](u.asInstanceOf[Union[R, Any]], Arrs.singleton(x => loop(i.continuation(x))))
        }
      }
    }

    loop(r)
  }
}

