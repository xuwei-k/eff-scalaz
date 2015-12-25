package org.specs2.control

import org.specs2.Specification
import org.specs2.control.Effects.|:
import MemberNat._
import scala.collection.mutable.ListBuffer
import EvalEffect._
import Eff._
import scalaz._, Scalaz._

class EvalEffectSpec extends Specification { def is = s2"""

 run is stack safe with Eval   $stacksafeRun

"""

  def stacksafeRun = {
    type E = Eval |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => EvalEffect.delay[E, Int](i))

    run(runEval(action)) ==== list
  }

  def stacksafeAttempt = {
    type E = Eval |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => EvalEffect.delay[E, Int](i))

    run(attemptEval(action)) ==== \/-(list)
  }
}
