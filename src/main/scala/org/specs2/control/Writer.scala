package org.specs2.control

import Eff._
import Effects._
import Member._

sealed trait Writer[O, X] {
  def value: O
}

case class Put[O](private val run: () => O) extends Writer[O, Unit] {
  def value: O =
    run()
}

object Writer {

  def put[O](o: => O): Writer[O, Unit] =
    Put(() => o)

  def tell[R, O](o: => O)(implicit member: Member[Writer[O, ?], R]): Eff[R, Unit] =
    // the type annotation is necessary here to prevent a compiler error
    send[Writer[O, ?], R, Unit](put(o))


  /**
   * runWriter :: Eff (Writer o ’: r ) a → Eff r (a,[ o])
   * runWriter =
   * handle relay (\x → return (x,[]))
   * (\(Put o) k → k () >>= \(x,l ) → return (x, o: l ))
   */
  def runWriter[R <: Effects, O, A](w: Eff[Writer[O, ?] <:: R, A]): Eff[R, (A, List[O])] = {
    val stater: Stater[Writer[O, ?], A, (A, List[O]), List[O]] = new Stater[Writer[O, ?], A, (A, List[O]), List[O]] {
      val init = List[O]()
      def apply[X](x: Writer[O, X], l: List[O]) = l :+ x.value
      def couple(a: A, l: List[O]) = (a, l)

    }

    interpretState1[R, Writer[O, ?], A, (A, List[O]), List[O]]((a: A) => (a, List[O]()))(stater)(w)
  }
}
