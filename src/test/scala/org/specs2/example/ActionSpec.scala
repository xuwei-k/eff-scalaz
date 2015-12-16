package org.specs2
package example

import Action._
import control.{Eval, Checked, Eff, Member}
import Eval._
import Checked.runChecked
import WarningsEff._
import ConsoleEff._
import Eff._
import Member.{<=}
import scalaz.{Writer => _, Reader => _,_}, Scalaz._, effect.IO

class ActionSpec extends Specification with ScalaCheck { def is = s2"""

 The action stack can be used to
   compute values                      $computeValues
   stop when there is an error         $stop
   display log messages                $logMessages
   collect warnings                    $collectWarnings
   emit a warning then fail            $warningAndFail

"""

  def computeValues =
    runWith(2, 3)._1 must beRight(5)

  def stop =
    runWith(20, 30)._1 must_== Left("too big")

  def logMessages = {
    val messages = new scala.collection.mutable.ListBuffer[String]
    runWith(1, 2, m => messages.append(m))

    messages.toList === List("got the value 1", "got the value 2")
  }

  def collectWarnings =
    runWith(2, 3)._2 must be_==(Vector("the sum is big: 5"))

  def warningAndFail = {
    val action = for {
      i <- eval(1)
      _ <- Action.warnAndFail("hmm", "let's stop")
    } yield i

    runAction(action)._1 must beLeft
  }


  /**
   * HELPERS
   */

  def runWith(i: Int, j: Int, printer: String => Unit = s => ()): (Either[String, Int], Vector[String]) =
    runAction(actions(i, j), printer)

  /** specifying the stack is enough to run it */
  def runWithUnbound(i: Int, j: Int, printer: String => Unit = s => ()): (Either[String, Int], Vector[String]) =
    runAction(unboundActions[ActionStack](i, j), printer)

  /**
   * ActionStack actions: no annotation is necessary here
   */
  def actions(i: Int, j: Int): Eff[ActionStack, Int] = for {
    x <- evalIO(IO(i))
    _ <- log("got the value "+x)
    y <- evalIO(IO(j))
    _ <- log("got the value "+y)
    s <- if (x + y > 10) Checked.ko("too big") else Checked.ok(x + y)
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
             m4: CheckedString <= R
  ): Eff[R, Int] = for {
    x <- evalIO[R, Int](IO(i))
    _ <- log[R]("got the value "+x)
    y <- evalIO[R, Int](IO(j))
    _ <- log[R]("got the value "+y)
    s <- if (x + y > 10) Checked.ko[R, String, Int]("too big") else Checked.ok[R, String, Int](x + y)
    _ <- if (s >= 5) warn[R]("the sum is big: "+s) else Eff.unit[R]
  } yield s

}
