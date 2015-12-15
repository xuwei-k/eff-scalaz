package org.specs2
package control

import Eff._
import Effects._

import scalaz.effect.IO


sealed trait Eval[A]

case class Evaluate[A](v: () => A) extends Eval[A] {
  def value: A = 
    v() 
} 

object Eval {
  
  def now[R <: Effects, A](a: =>A)(implicit m: Member[Eval, R]): Eff[R, A] = 
    pure(a)

  def eval[R <: Effects, A](a: =>A)(implicit m: Member[Eval, R]): Eff[R, A] = 
    impure(m.inject(Evaluate[A](() => a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))
  
  def evalIO[R <: Effects, A](a: IO[A])(implicit m: Member[Eval, R]): Eff[R, A] =
    eval(a.unsafePerformIO)
  
  def runEval[R <: Effects, A](r: Eff[Eval[?] <:: R, A]): Eff[R, A] = {
    val runPure = (a: A) => EffMonad[R].point(a)

    val runImpure = new EffCont[Eval, R, A] {
      def apply[X](r: Eval[X])(continuation: X => Eff[R, A]): Eff[R, A] = r match {
        case e@Evaluate(_) => continuation(e.value)
      }
    }

    relay[R, Eval, A, A](runPure, runImpure)(r)
  }
    
}

