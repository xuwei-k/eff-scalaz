package org.specs2.control

import org.specs2.control.MemberNat._

import scalaz._, Scalaz._
import Eff._
import Effects._
import Member._

sealed trait Writer[O, X]

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
    val putRest = new EffCont[Writer[O, ?], R, (A, List[O])] {
      def apply[X](w: Writer[O, X])(continuation: X => Eff[R, (A, List[O])]): Eff[R, (A, List[O])] = w match {
        case p @ Put(_) => continuation(()) >>= ((xl: (A, List[O])) => EffMonad.point((xl._1, p.value +: xl._2)))
      }
    }

    relay1[R, Writer[O, ?], A, (A, List[O])]((a: A) => (a, List[O]()))(putRest)(w)
  }

  }
