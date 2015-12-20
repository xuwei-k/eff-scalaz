package org.specs2.control

import Eff._
import Effects._
import Member._

sealed trait State[S, X]

case class Get[S]() extends State[S, S]
case class Put[S](s: S) extends State[S, Unit]

object State {

  def put[R, S](s: S)(implicit member: Member[State[S, ?], R]): Eff[R, Unit] =
    send[State[S, ?], R, Unit](Put(s))

  def get[R, S](implicit member: Member[State[S, ?], R]): Eff[R, S] =
    send[State[S, ?], R, S](Get())

  def runState[R <: Effects, S, A](initial: S)(w: Eff[State[S, ?] <:: R, A]): Eff[R, (A, S)] = {
    val stater: Stater[State[S, ?], A, (A, S), S] = new Stater[State[S, ?], A, (A, S), S] {
      val init = initial
      def apply[X](x: State[S, X], s: S) = x match {
        case Get()   => (s.asInstanceOf[X], s)
        case Put(s1) => ((), s1)
      }
      def couple(a: A, s: S) = (a, s)

    }

    interpretState1[R, State[S, ?], A, (A, S), S]((a: A) => (a, initial))(stater)(w)
  }
}
