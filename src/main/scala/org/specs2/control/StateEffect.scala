package org.specs2.control

import Eff._
import Effects._
import scalaz._
import Interpret._

/**
 * Effect for passing state along computations
 *
 * Several state effects can be used in the same stack if they are tagged
 *
 * Internally backed up by scalaz.State
 *
 */
object StateEffect {

  /** store a new state value */
  def put[R, S](s: S)(implicit member: Member[State[S, ?], R]): Eff[R, Unit] =
    send[State[S, ?], R, Unit](Scalaz.put(s))

  /** get the current state value */
  def get[R, S](implicit member: Member[State[S, ?], R]): Eff[R, S] =
    send[State[S, ?], R, S](Scalaz.get)

  /** run a state effect, with an initial value */
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

  /** run a tagged state effect, with an initial value */
  def runTaggedState[R <: Effects, T, S1, A](initial: S1)(w: Eff[({type l[X] = State[S1, X] @@ T})#l |: R, A]): Eff[R, (A, S1)] = {
    type SS[X] = State[S1, X] @@ T

    val recurse: StateRecurse[SS, A, (A, S1)] = new StateRecurse[SS, A, (A, S1)] {
      type S = S1
      val init = initial

      def apply[X](x: State[S, X] @@ T, s: S) =
        Tag.unwrap(x)(s).swap

      def finalize(a: A, s: S) = (a, s)

    }

    interpretState1[R, SS, A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }
}
