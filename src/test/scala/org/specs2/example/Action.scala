package org.specs2.example

import org.specs2.control.{Eff, Effects, Member, Checked, MemberNat, Eval, EffectsNil}
import Effects._, Eff._

import scalaz.{Reader => _, Writer => _, _}, Scalaz._
import WarningsEff._
import ConsoleEff._
import Member._
import MemberNat._

object Action {

  type CheckedString[A] = Checked[String, A]

  type ActionStack = Console <:: Warnings <:: CheckedString <:: Eval <:: EffectsNil

  implicit def EvalEffect: Member[Eval, ActionStack] =
    Member.MemberNatIsMember

  implicit def CheckedStringEffect: Member[CheckedString, ActionStack] =
    Member.MemberNatIsMember

  implicit def WarningsEffect: Member[Warnings, ActionStack] =
    Member.MemberNatIsMember


  implicit def ConsoleEffect: Member[Console, ActionStack] =
    Member.MemberNatIsMember

  /**
   * warn the user about something that is probably wrong on his side,
   * this is not a specs2 bug, then fail to stop all further computations
   */
  def warnAndFail[R <: Effects, A](message: String, failureMessage: String)(implicit m1: Warnings <= R, m2: CheckedString <= R): Eff[R, A] =
    warn(message)(m1) >>
    Checked.ko(failureMessage)
}
