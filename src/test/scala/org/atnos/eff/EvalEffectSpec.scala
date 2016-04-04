package org.atnos.eff

import org.specs2.Specification
import Eff._
import Effects._
import EvalEffect._

import scalaz._, Scalaz._

class EvalEffectSpec extends Specification { def is = s2"""

 run is stack safe with Eval   $stacksafeRun

"""

  type E = Eval |: NoEffect

  val list = (1 to 5000).toList

  def stacksafeRun = {
    val action = list.traverseU(i => EvalEffect.delay(i))
    run(runEval(action)) ==== list
  }

  def stacksafeAttempt = {
    val action = list.traverseU(i => EvalEffect.delay(i))
    run(attemptEval(action)) ==== \/-(list)
  }
}

