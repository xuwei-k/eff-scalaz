package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.syntax.all._
import org.atnos.eff._, all._
import futurez._
import org.atnos.eff.syntax.futurez._

import scala.concurrent.duration._
import scalaz._, concurrent._

class FuturezEffectSpec extends Specification { def is = s2"""

 A future effect can be added to a stack of effects $e1

"""

  def e1 = {
    type S = Fx.fx2[Future, Option]

    val action: Eff[S, Int] = for {
      a <- delayed[S, Int](10)
      b <- option.some[S, Int](a)
    } yield a + b

    action.runOption.attemptFuture(1.second).run ==== \/.right(Some(20))

  }
}
