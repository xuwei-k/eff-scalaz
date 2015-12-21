package org.specs2.control

import Eff._
import Effects._
import Member._

/**
 * Effect for logging values alongside computations
 */
object Writer {

  sealed trait Writer[O, X] {
    def value: O
  }

  case class Write[O](o: O) extends Writer[O, Unit] {
    def value: O =
      o
  }

  def write[O](o: O): Writer[O, Unit] =
    Write(o)

  def tell[R, O](o: O)(implicit member: Member[Writer[O, ?], R]): Eff[R, Unit] =
    // the type annotation is necessary here to prevent a compiler error
    send[Writer[O, ?], R, Unit](write(o))

  def runWriter[R <: Effects, O, A](w: Eff[Writer[O, ?] <:: R, A]): Eff[R, (A, List[O])] = {
    val recurse: StateRecurse[Writer[O, ?], A, (A, List[O])] = new StateRecurse[Writer[O, ?], A, (A, List[O])] {
      type S = List[O]
      val init = List[O]()
      def apply[X](x: Writer[O, X], l: List[O]) = (().asInstanceOf[X], l :+ x.value)
      def finalize(a: A, l: List[O]) = (a, l)
    }

    interpretState1[R, Writer[O, ?], A, (A, List[O])]((a: A) => (a, List[O]()))(recurse)(w)
  }
}
