package org.specs2.control

import Eff._
import Effects._
import Member._

/**
 * Effect for passing state
 */
object State {

  sealed trait State[S, X]

  case class Get[S]() extends State[S, S]
  case class Put[S](s: S) extends State[S, Unit]

  def put[R, S](s: S)(implicit member: Member[State[S, ?], R]): Eff[R, Unit] =
    send[State[S, ?], R, Unit](Put(s))

  def get[R, S](implicit member: Member[State[S, ?], R]): Eff[R, S] =
    send[State[S, ?], R, S](Get())

  def runState[R <: Effects, S1, A](initial: S1)(w: Eff[State[S1, ?] <:: R, A]): Eff[R, (A, S1)] = {
    val recurse: StateRecurse[State[S1, ?], A, (A, S1)] = new StateRecurse[State[S1, ?], A, (A, S1)] {
      type S = S1
      val init = initial
      def apply[X](x: State[S, X], s: S) = x match {
        case Get()   => (s.asInstanceOf[X], s)
        case Put(s1) => ((), s1)
      }
      def finalize(a: A, s: S) = (a, s)

    }

    interpretState1[R, State[S1, ?], A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }
}
