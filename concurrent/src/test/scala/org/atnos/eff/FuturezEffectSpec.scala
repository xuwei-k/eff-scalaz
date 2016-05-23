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
    type S = Future |: Option |: NoEffect
    implicit val f: Member.Aux[Future, S, Option |: NoEffect] = Member.first
    implicit val o: Member.Aux[Option, S, Future |: NoEffect] = Member.successor

    val action: Eff[S, Int] = for {
      a <- delayed(10)
      b <- option.some(a)
    } yield a + b

    action.runOption.attemptFuture(1.second).run ==== \/.right(Some(20))

  }
}
