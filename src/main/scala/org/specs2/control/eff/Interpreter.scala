package org.specs2.control.eff

import org.specs2.control.eff.Effects._
import scalaz._, Scalaz._

trait Interpreter[M[_], Out[_]] {
  def interpret[E <: Effects, A](eff: Eff[M |: E, A]): Eff[E, Out[A]]
}

object Interpreter {

  type ::[H[_], T <: Effects, I[_], O[_]] =
    InterpreterCons[H, T, I, O]

  val :: = InterpreterCons

}

sealed trait InterpreterStack[E, Out[_]] {
  def interpret[A](eff: Eff[E, A]): Out[A]
}

case class InterpreterCons[M[_], E <: Effects, I[_], O[_]](head: Interpreter[M, I], tail: InterpreterStack[E, O]) extends InterpreterStack[M |: E, ({type l[A]=O[I[A]]})#l] { stack =>
  def interpret[A](eff: Eff[M |: E, A]): O[I[A]] =
    tail.interpret(head.interpret(eff))

  type Out[A] = O[I[A]]

  def ::[N[_], J[_]](i: Interpreter[N, J]) =
    InterpreterCons[N, M |: E, J, Out](i, stack)

}


object NoEffectInterpreter extends InterpreterStack[NoEffect, Id] { stack =>
  def ::[N[_], J[_]](i: Interpreter[N, J]) =
    InterpreterCons[N, NoEffect, J, Id](i, stack)

  def interpret[A](eff: Eff[NoEffect, A]): Id[A] =
    Eff.run(eff)

}
