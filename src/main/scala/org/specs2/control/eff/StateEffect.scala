package org.specs2.control.eff

import Eff._
import Effects._
import Interpret._
import scalaz.{State => _, _}, Scalaz._

/**
 * Effect for passing state along computations
 *
 * Several state effects can be used in the same stack if they are tagged
 *
 *
 */
object StateEffect {

  sealed class State[S, A] {
    def apply(s: S): (S, A)
  }
  case class Put[S](s: S) extends State[S, Unit] {
    def apply(s: S) = (s, ())
  }
  case class Get[S]() extends State[S, S]  {
    def apply(s: S) = (s, s)
  }
  case class Gets[S, A](f: S => A) extends State[S, A] {
    def apply(s: S) = (s, f(s))
  }

  /** store a new state value */
  def put[R, S](s: S)(implicit member: Member[State[S, ?], R]): Eff[R, Unit] =
    send[State[S, ?], R, Unit](Put(s))

  /** get the current state value */
  def get[R, S](implicit member: Member[State[S, ?], R]): Eff[R, S] =
    send[State[S, ?], R, S](Get())

  /** get the current state value and map it with a function f */
  def gets[R, S, T](f: S => T)(implicit member: Member[State[S, ?], R]): Eff[R, T] =
    send[State[S, ?], R, T](Gets(f))

  /** modify the current state value */
  def modify[R, S](f: S => S)(implicit member: Member[State[S, ?], R]): Eff[R, Unit] =
    get >>= ((s: S) => put(f(s)))

  /** run a state effect, with a Monoidal state */
  def evalZero[R <: Effects, S: Monoid, A](w: Eff[State[S, ?] |: R, A]): Eff[R, A] =
    eval(Monoid[S].zero)(w)

  /** run a state effect, with an initial value, return only the value */
  def eval[R <: Effects, S, A](initial: S)(w: Eff[State[S, ?] |: R, A]): Eff[R, A] =
    runState(initial)(w).map(_._1)

  /** run a state effect, with a monoidal state, return only the state */
  def execZero[R <: Effects, S : Monoid, A](w: Eff[State[S, ?] |: R, A]): Eff[R, S] =
    exec(Monoid[S].zero)(w)

  /** run a state effect, with an initial value, return only the state */
  def exec[R <: Effects, S, A](initial: S)(w: Eff[State[S, ?] |: R, A]): Eff[R, S] =
    runState(initial)(w).map(_._2)

  /** run a state effect, with an initial value */
  def runStateZero[R <: Effects, S : Monoid, A](w: Eff[State[S, ?] |: R, A]): Eff[R, (A, S)] =
    runState(Monoid[S].zero)(w)

  /** optimize state effects when there are consecutive puts or gets */
  def optimizeState[R <: Effects, S1, A](w: Eff[State[S1, ?] |: R, A]): Eff[State[S1, ?] |: R, A] = {
    def loop(e: Eff[State[S1, ?] |: R, A], last: Option[State[S1, _]]): Eff[State[S1, ?] |: R, A] = {
      w match {
        case Pure(_) => e
        case Impure(union, continuation) =>
          decompose(union) match {
            case -\/(_) => e

            case \/-(ms) =>
              (ms, last) match {
                case (Put(_), Some(Put(_))) =>

                case (Put(s), _) =>
                  loop(continuation(s), Some(ms))

                case (Get(), _) =>
                  loop(continuation(s), Some(ms))
              }
          }
      }
    }

    loop(w, last = None)
  }


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
