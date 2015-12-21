package org.specs2.example

import org.specs2.control.DisjunctionEffect._
import org.specs2.control.EvalEffect._
import org.specs2.control.{Effects, Eff, DisjunctionErrorEffect, Member, MemberNat, EvalEffect, EffectsNil, Pure, Impure}
import Effects._, Eff._

import scala.util.control.NonFatal
import scalaz.{Reader => _, Writer => _, _}, Scalaz._
import WarningsEffect._
import ConsoleEffect._
import DisjunctionErrorEffect._
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
 *  DisjunctionString <:: Console <:: Warnings <:: Eval <:: EffectsNil
 *
 *  will return warnings *and* failures: (String \/ A, Vector[String])
 *
 * Whereas
 *
 *  Console <:: Warnings <:: DisjunctionString <:: Eval <:: EffectsNil
 *
 *  will return not warnings if there is a failure: String \/ (A, Vector[String])
 *
 * Also note that Eval is the last effect which means that nothing get evaluated until we run the last interpreter
 *
 */
object Action {

  type ActionStack = DisjunctionError <:: Console <:: Warnings <:: Eval <:: EffectsNil

  implicit def EvalMember: Member[Eval, ActionStack] =
    Member.MemberNatIsMember

  implicit def WarningsMember: Member[Warnings, ActionStack] =
    Member.MemberNatIsMember

  implicit def ConsoleMember: Member[Console, ActionStack] =
    Member.MemberNatIsMember

  implicit def DisjunctionErrorMember: Member[DisjunctionError, ActionStack] =
    Member.MemberNatIsMember

  /**
   * warn the user about something that is probably wrong on his side,
   * and then fail all other computations
   */
  def warnAndFail[R <: Effects, A](message: String, failureMessage: String)(implicit m1: Warnings <= R, m2: DisjunctionError <= R): Eff[R, A] =
    warn(message)(m1) >>
    fail(failureMessage)

  def runAction[A](action: Eff[ActionStack, A], printer: String => Unit = s => ()): (Error \/ A, Vector[String]) =
    run(runEval(runWarnings(runConsoleToPrinter(printer)(runDisjunction(action)))))


}
