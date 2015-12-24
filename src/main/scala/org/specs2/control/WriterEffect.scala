package org.specs2.control

import Eff._
import Effects._
import Member._
import Interpret._
import scalaz._, Scalaz._
import collection.mutable._

/**
 * Effect for logging values alongside computations
 *
 * Compared to traditional Writer monad which accumulates values by default
 * this effect can be interpreted in different ways:
 *
 *  - log values to the console or to a file as soon as they are produced
 *  - accumulate values in a list
 *
 * Several writer effects can be used in the same stack if they are tagged.
 *
 */
object WriterEffect {

  /** write a give value */
  def tell[R, O](o: O)(implicit member: Member[Writer[O, ?], R]): Eff[R, Unit] =
    send[Writer[O, ?], R, Unit](Writer(o, ()))

  /**
   * run a writer effect and return the list of written values
   *
   * This uses a ListBuffer internally to append values
   */
  def runWriter[R <: Effects, O, A](w: Eff[Writer[O, ?] |: R, A]): Eff[R, (A, List[O])] = {
    val recurse: StateRecurse[Writer[O, ?], A, (A, ListBuffer[O])] = new StateRecurse[Writer[O, ?], A, (A, ListBuffer[O])] {
      type S = ListBuffer[O]
      val init = new ListBuffer[O]
      def apply[X](x: Writer[O, X], l: ListBuffer[O]) = (x.run._2, l :+ x.run._1)
      def finalize(a: A, l: ListBuffer[O]) = (a, l)
    }

    interpretState1[R, Writer[O, ?], A, (A, ListBuffer[O])]((a: A) => (a, new ListBuffer[O]))(recurse)(w).map {
      case (a, l) => (a, l.toList)
    }
  }

  /**
   * run a tagged writer effect
   */
  def runTaggedWriter[R <: Effects, T, O, A](w: Eff[({type l[X] = Writer[O, X] @@ T})#l |: R, A]): Eff[R, (A, Vector[O])] = {
    type W[X] = Writer[O, X] @@ T

    val recurse = new StateRecurse[W, A, (A, Vector[O])] {
      type S = Vector[O]
      val init = Vector()

      def apply[X](xt: W[X], s: Vector[O]): (X, Vector[O]) =
        Tag.unwrap(xt) match {
          case x => (x.run._2, s :+ x.run._1)
        }

      def finalize(a: A, s: Vector[O]): (A, Vector[O]) =
        (a, s)
    }

    interpretState1[R, W, A, (A, Vector[O])]((a: A) => (a, Vector()))(recurse)(w)
  }

}
