package org.specs2
package example

import Action._
import control.{ErrorEffect, EvalEffect, Member, Eff}
import EvalEffect._
import WarningsEffect._
import ConsoleEffect._
import ErrorEffect._
import Member.{<=}
import org.specs2.matcher.DisjunctionMatchers
import scalaz._, Scalaz._, effect.IO

class ActionSpec extends Specification with ScalaCheck with DisjunctionMatchers { def is = s2"""

 The action stack can be used to
   compute values                      $computeValues
   stop when there is an error         $stop
   display log messages                $logMessages
   collect warnings                    $collectWarnings
   emit a warning then fail            $warningAndFail
   do an action or else warn           $orElseWarn

"""

  def computeValues =
    runWith(2, 3)._1 must be_\/-(5)

  def stop =
    runWith(20, 30)._1 ==== -\/(\/-("too big"))

  def logMessages = {
    val messages = new scala.collection.mutable.ListBuffer[String]
    runWith(1, 2, m => messages.append(m))

    messages.toList === List("got the value 1", "got the value 2")
  }

  def collectWarnings =
    runWith(2, 3)._2 must be_==(Vector("the sum is big: 5"))

  def warningAndFail = {
    val action = for {
       i <- EvalEffect.delay(1)
       _ <- Action.warnAndFail("hmm", "let's stop")
      } yield i

    runAction(action)._1 must be_-\/
  }

  def orElseWarn = {
    val action =
      ErrorEffect.fail("failed").orElse(warn("that didn't work"))

    runAction(action)._1 must be_\/-
  }

  /**
   * HELPERS
   */

  def runWith(i: Int, j: Int, printer: String => Unit = s => ()): (Error \/ Int, List[String]) =
    runAction(actions(i, j), printer)

  /** specifying the stack is enough to run it */
  def runWithUnbound(i: Int, j: Int, printer: String => Unit = s => ()): (Error \/ Int, List[String]) =
    runAction(unboundActions[ActionStack](i, j), printer)

  /**
   * ActionStack actions: no annotation is necessary here
   */
  def actions(i: Int, j: Int): Eff[ActionStack, Int] = for {
    x <- evalIO(IO(i))
    _ <- log("got the value "+x)
    y <- evalIO(IO(j))
    _ <- log("got the value "+y)
    s <- if (x + y > 10) fail("too big") else ErrorEffect.ok(x + y)
    _ <- if (s >= 5) warn("the sum is big: "+s) else Eff.unit[ActionStack]
  } yield s

  /**
   * "open" effects version of the same actions
   * this one can be reused with more effects
   */
  def unboundActions[R](i: Int, j: Int)(
    implicit m1: Eval <= R,
             m2: Console <= R,
             m3: Warnings <= R,
             m4: ErrorOrOk <= R
  ): Eff[R, Int] = for {
    x <- evalIO[R, Int](IO(i))
    _ <- log[R]("got the value "+x)
    y <- evalIO[R, Int](IO(j))
    _ <- log[R]("got the value "+y)
    s <- if (x + y > 10) fail[R, Int]("too big") else ErrorEffect.ok[R, Int](x + y)
    _ <- if (s >= 5) warn[R]("the sum is big: "+s) else Eff.unit[R]
  } yield s

}
