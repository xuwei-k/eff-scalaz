package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.syntax.all._
import all._

import scala.concurrent._
import duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.specs2.concurrent.ExecutionEnv
import scalaz._
import Scalaz._
import scala.collection.mutable.ListBuffer
import org.specs2.matcher.DisjunctionMatchers._

class FutureEffectSpec(implicit ee: ExecutionEnv) extends Specification { def is = s2"""

 A future effect can be added to a stack of effects $e1
 A future execution can be delayed $e2

 A Future value execution can be delayed $e3

 A Future can be lifted to a stack of effects $e4

 We can use a partial function to recover from an exception $e5

 We can attempt a future and use an \/ effect for the exception $e6

"""

  type S = Fx.fx2[Future, Eval]

  def e1 = {
    def action[R :_future :_option]: Eff[R, Int] = for {
      a <- async(10)
      b <- option.some(a)
    } yield a + b

    action[Fx.fx2[Future, Option]].runOption.awaitFuture(1.second).run ==== \/.right(Some(20))
  }

  def e2 = {
    def action[R :_future :_eval]: Eff[R, Int] =
      delay(10).flatMap(v => async(v))

    action[S].runEval.awaitFuture(1.second).run ==== \/.right(10)
  }

  def e3 = {
    def action[R :_future :_eval]: Eff[R, Int] =
      delay(Future(10)).flatMap(v => send(v))

    action[S].runEval.awaitFuture(1.second).run ==== \/.right(10)
  }

  def e4 = {

    def future: Future[Int] = Future(10)
    def action[R :_future :_eval]: Eff[R, Int] = future.liftFuture

    action[S].runEval.awaitFuture(1.second).run ==== \/.right(10)
  }

  def e5 = {
    type WriterString[A] = Writer[String, A]
    type _log[R] = WriterString |= R

    def action[R :_future :_log] =
      tell("message") >>
      async(throwException)

    type S1 = Fx.fx2[WriterString, Future]

    val messages = new ListBuffer[String]
    val action1 = send(action[S1].runWriterUnsafe((s: String) => messages.append(s)).detach.recover { case e: TimeoutException => 2 })

    (action1.detach must be_==(2).await) and
     (messages.toList === List("message"))

  }

  def e6 = {
    def action[R :_future :_eval :_throwableOr] =
      FutureEffect.attemptFuture(Future(throwException))

    type S1 = Fx.fx3[ThrowableOr, Eval, Future]

    action[S1].runDisjunction.runEval.detach must be_-\/((e: Throwable) => e must haveClass[TimeoutException]).await
  }

  /** throw an exception with no dead code warning */
  def throwException: Int =
    if ("1".toInt == 1) throw new TimeoutException("boom") else 1

}
