package org.atnos.eff

import org.atnos.eff.all._
import org.atnos.eff.task._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.task._
import org.specs2.Specification

import scala.concurrent.duration._
import scalaz._
import scalaz.concurrent._

class TaskEffectSpec extends Specification { def is = s2"""

 A Task effect can be added to a stack of effects $e1

"""

  def e1 = {
    type S = Fx.fx2[Task, Option]

    val action: Eff[S, Int] = for {
      a <- doLater[S, Int](10)
      b <- option.some[S, Int](a)
    } yield a + b

    action.runOption.attemptTask(1.second).run ==== \/.right(Some(20))

  }
}
