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

 An action can be evaluated, with another one
   if the first is successful, the second is not executed $orElse1
   if the first is not successful, the second is executed $orElse2


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

  def orElse1 = {
    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      Eval.delay[R, Int] { messages.append("first"); 1 }

    val all: Eff[R, Int] =
      action.orElse(Eval.delay { messages.append("second"); 2 })

    (run(runEval(all)) === 1) and
    (messages.toList === List("first"))
  }

  def orElse2 = {
    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      Eval.delay[R, Int] { throw new Exception("boom"); messages.append("first"); 1 }

    val all: Eff[R, Int] =
      action.orElse(Eval.delay { messages.append("second"); 2 })

    (run(runEval(all)) === 2) and
    (messages.toList === List("second"))
  }
}
