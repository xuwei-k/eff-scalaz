package org.specs2.control

import org.specs2.Specification
import org.specs2.control.Effects.<::
import Member._
import scala.collection.mutable.ListBuffer
import Eval._
import Eff._

class EvalSpec extends Specification { def is = s2"""

 An action can be evaluated after another
   when the first action is ok   $andFinallyOk
   even if there is an exception $andFinallyKo

"""

  type R = Eval <:: EffectsNil
  def andFinallyOk = {

    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      Eval.delay[R, Unit](messages.append("first"))

    val all: Eff[R, Unit] =
      action.andFinally(Eval.delay(messages.append("final")))


    run(runEval(all))

    messages.toList === List("first", "final")
  }

  def andFinallyKo = {

    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      Eval.delay[R, Unit] { throw new Exception("boom"); messages.append("first") }

    val all: Eff[R, Unit] =
      action.andFinally(Eval.delay(messages.append("final")))


    run(attemptEval(all))

    messages.toList === List("final")
  }
}
