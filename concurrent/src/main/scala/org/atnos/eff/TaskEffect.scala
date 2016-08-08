package org.atnos.eff

import java.util.concurrent.ExecutorService

import scala.util.control.NonFatal
import Eff._
import Interpret._

import scalaz._
import scalaz.concurrent._
import scala.concurrent.duration._

/**
 * Effect for scalaz.Task computations
 */
trait TaskEffect extends
  TaskCreation with
  TaskInterpretation

object TaskEffect extends TaskEffect

trait TaskCreation {

  type _Task[R] = Task <= R
  type _task[R] = Task |= R
  
  def doNow[R, A](a: A)(implicit m: Task |= R): Eff[R, A] =
    pure(a)

  def doLater[R, A](a: =>A)(implicit m: Task |= R): Eff[R, A] =
    send(Task.delay(a))

  def doFork[R, A](a: =>Task[A])(implicit m: Task |= R, pool: ExecutorService = Strategy.DefaultExecutorService): Eff[R, A] =
    send(Task.fork(a))

  def doTask[R, A](a: Task[A])(implicit m: Task |= R): Eff[R, A] =
    send(a)
}

trait TaskInterpretation {

  def attemptTask[R, U, A](r: Eff[R, A])(atMost: Duration)
      (implicit m: Member.Aux[Task, R, U]): Eff[U, Throwable \/ A] = {
    val recurse = new Recurse[Task, U, Throwable \/ A] {
      def apply[X](m: Task[X]) =
        try { m.unsafePerformSyncAttemptFor(atMost).swap.map(e => Eff.pure(\/.left(e))) }
        catch { case NonFatal(t) => \/-(Eff.pure(-\/(t))) }
    }

    interpret1((a: A) => \/-(a): Throwable \/ A)(recurse)(r)
  }

}

object TaskInterpretation extends TaskInterpretation

