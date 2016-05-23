package org.atnos.eff.syntax

import org.atnos.eff._
import scala.concurrent.duration._
import scalaz.concurrent._
import scalaz.\/

object task extends task

trait task {

  implicit class TaskEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def attemptTask[U <: Effects](atMost: Duration)
      (implicit member: Member.Aux[Task, R, U]): Eff[U, Throwable \/ A] =
      TaskInterpretation.attemptTask(e)(atMost)

  }

}
