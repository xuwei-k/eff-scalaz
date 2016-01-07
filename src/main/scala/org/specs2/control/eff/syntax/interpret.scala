package org.specs2.control.eff.syntax

import org.specs2.control.eff.Effects._
import org.specs2.control.eff.{InterpreterStack, Eff, Effects}

object interpret {

  implicit class InterpretEffect[M[_], E <: Effects, A](eff: Eff[M |: E, A]) {
    def interpret[Out[_]](stack: InterpreterStack[M |: E, Out]): Out[A] =
      stack.interpret(eff)
  }

}
