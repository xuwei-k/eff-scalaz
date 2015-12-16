package org.specs2.example

import org.specs2.control.{Eff, Effects, Member, Checked, Union, Writer, Eval, EffectsNil, UnionNext, UnionNow}
import Effects._, Eff._

import scalaz.{Reader => _, Writer => _, _}, Scalaz._
import WarningsEff._
import ConsoleEff._
import Member._

object Action {

  type CheckedString[A] = Checked[String, A]

  type ActionStack = Console <:: Warnings <:: CheckedString <:: Eval <:: EffectsNil

  implicit def EvalEffect: Member[Eval, ActionStack] = new Member[Eval, ActionStack] {
    def inject[V](tv: Eval[V]): Union[ActionStack, V] =
      UnionNext(UnionNext(UnionNext(UnionNow(tv))))
    def project[V](u: Union[ActionStack, V]): Option[Eval[V]] =
      u match {
        case UnionNow(x) => None
        case UnionNext(UnionNext(UnionNext(UnionNow(e)))) => Some(e)
      }
  }

  implicit def CheckedStringEffect: Member[CheckedString, ActionStack] = new Member[CheckedString, ActionStack] {
    def inject[V](tv: CheckedString[V]): Union[ActionStack, V] =
      UnionNext(UnionNext(UnionNow(tv)))
    def project[V](u: Union[ActionStack, V]): Option[CheckedString[V]] =
      u match {
        case UnionNow(x) => None
        case UnionNext(UnionNext(UnionNow(e))) => Some(e)
      }
  }

  implicit def WarningsEffect: Member[Warnings, ActionStack] = new Member[Warnings, ActionStack] {
    def inject[V](tv: Warnings[V]): Union[ActionStack, V] =
      UnionNext(UnionNow(tv))
    def project[V](u: Union[ActionStack, V]): Option[Warnings[V]] =
      u match {
        case UnionNow(x) => None
        case UnionNext(UnionNow(e)) => Some(e)
      }
  }

  implicit def ConsoleEffect: Member[Console, ActionStack] = new Member[Console, ActionStack] {
    def inject[V](tv: Console[V]): Union[ActionStack, V] =
      Union.now(tv)
    def project[V](u: Union[ActionStack, V]): Option[Console[V]] =
      u match {
        case UnionNow(x) => Some(x)
        case _ => None
      }
  }

  /**
   * warn the user about something that is probably wrong on his side,
   * this is not a specs2 bug, then fail to stop all further computations
   */
  def warnAndFail[R <: Effects, A](message: String, failureMessage: String)(implicit m1: Warnings <= R, m2: CheckedString <= R): Eff[R, A] =
    warn(message)(m1) >>
    Checked.ko(failureMessage)
}
