package org.specs2.control

import Eff._
import Effects._
import Member._
import scalaz._
import Interpret._

/**
 * Effect for passing state
 */
object StateEffect {

  def put[R, S](s: S)(implicit member: Member[State[S, ?], R]): Eff[R, Unit] =
    send[State[S, ?], R, Unit](Scalaz.put(s))

  def get[R, S](implicit member: Member[State[S, ?], R]): Eff[R, S] =
    send[State[S, ?], R, S](Scalaz.get)

  def runState[R <: Effects, S1, A](initial: S1)(w: Eff[State[S1, ?] |: R, A]): Eff[R, (A, S1)] = {
    val recurse: StateRecurse[State[S1, ?], A, (A, S1)] = new StateRecurse[State[S1, ?], A, (A, S1)] {
      type S = S1
      val init = initial

      def apply[X](x: State[S, X], s: S) =
        x(s).swap

      def finalize(a: A, s: S) = (a, s)

    }

    interpretState1[R, State[S1, ?], A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }
}
