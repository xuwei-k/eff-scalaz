package org.specs2.control

import Eff._
import Effects._
import Member._
import Interpret._
import scalaz._

/**
 * Effect for logging values alongside computations
 */
object WriterEffect {

  def write[O](o: O): Writer[O, Unit] =
    Writer(o, ())

  def tell[R, O](o: O)(implicit member: Member[Writer[O, ?], R]): Eff[R, Unit] =
    send[Writer[O, ?], R, Unit](write(o))

  def runWriter[R <: Effects, O, A](w: Eff[Writer[O, ?] |: R, A]): Eff[R, (A, List[O])] = {
    val recurse: StateRecurse[Writer[O, ?], A, (A, List[O])] = new StateRecurse[Writer[O, ?], A, (A, List[O])] {
      type S = List[O]
      val init = List[O]()
      def apply[X](x: Writer[O, X], l: List[O]) = (x.run._2, l :+ x.run._1)
      def finalize(a: A, l: List[O]) = (a, l)
    }

    interpretState1[R, Writer[O, ?], A, (A, List[O])]((a: A) => (a, List[O]()))(recurse)(w)
  }

  def runTaggedWriter[R <: Effects, T, O, A](w: Eff[({type l[X] = Writer[O, X] @@ T})#l |: R, A]): Eff[R, (A, Vector[O])] = {
    val recurse = new StateRecurse[({type l[X] = Writer[O, X] @@ T})#l, A, (A, Vector[O])] {
      type S = Vector[O]
      val init = Vector()

      def apply[X](xt: Writer[O, X] @@ T, s: Vector[O]): (X, Vector[O]) =
        Tag.unwrap(xt) match {
          case x => (x.run._2, s :+ x.run._1)
        }

      def finalize(a: A, s: Vector[O]): (A, Vector[O]) =
        (a, s)
    }

    interpretState1[R, ({type l[X] = Writer[O, X] @@ T})#l, A, (A, Vector[O])]((a: A) => (a, Vector()))(recurse)(w)
  }

}
