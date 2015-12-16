package org.specs2.example

import org.specs2.control.Checked._
import org.specs2.control.Eval._
import org.specs2.control.{Eff, Effects, Member, Checked, MemberNat, Eval, EffectsNil}
import Effects._, Eff._

import scalaz.{Reader => _, Writer => _, _}, Scalaz._
import WarningsEff._
import ConsoleEff._
import Member._
import MemberNat._


/**
 * This is an example of a stack of effect with:
 *
 *  - 2 different writers: one for warnings, the other one for logging to the console
 *  - one "IO" effect
 *  - one Error effect
 *
 * The order of the effects in the stack definition is important.
 *
 * For example
 *
 *  CheckedString <:: Console <:: Warnings <:: Eval <:: EffectsNil
 *
 *  will return warnings *and* failures: (Either[String, A], Vector[String])
 *
 * Whereas
 *
 *  Console <:: Warnings <:: CheckedString <:: Eval <:: EffectsNil
 *
 *  will return not warnings if there is a failure: Either[String, (A, Vector[String])]
 *
 * Also note that Eval is the last effect which means that nothing get evaluated until we run the last interpreter
 *
 */
object Action {

  type CheckedString[A] = Checked[String, A]

  type ActionStack = CheckedString <:: Console <:: Warnings <:: Eval <:: EffectsNil

  implicit def EvalEffect: Member[Eval, ActionStack] =
    Member.MemberNatIsMember

  implicit def WarningsEffect: Member[Warnings, ActionStack] =
    Member.MemberNatIsMember

  implicit def ConsoleEffect: Member[Console, ActionStack] =
    Member.MemberNatIsMember

  implicit def CheckedStringEffect: Member[CheckedString, ActionStack] =
    Member.MemberNatIsMember

  /**
   * warn the user about something that is probably wrong on his side,
   * and then fail all other computations
   */
  def warnAndFail[R <: Effects, A](message: String, failureMessage: String)(implicit m1: Warnings <= R, m2: CheckedString <= R): Eff[R, A] =
    warn(message)(m1) >>
    Checked.ko(failureMessage)

  def runAction[A](action: Eff[ActionStack, A], printer: String => Unit = s => ()): (Either[String, A], Vector[String]) =
    run(runEval(runWarnings(runConsoleToPrinter(printer)(runChecked(action)))))


}
