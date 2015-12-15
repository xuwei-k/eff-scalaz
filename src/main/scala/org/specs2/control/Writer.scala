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

  def tell[R <: Effects, O](o: => O)(implicit member: Member[Writer[O, ?], R]): Eff[R, Unit] =
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

  type WriterStack[O, E <: Effects] = Writer[O, ?] <:: E

  implicit def WriterMember[R <: Effects, A]: Member[Writer[A, ?], Writer[A, ?] <:: R] = 
    Member.MemberNatIsMember[Writer[A, ?], Writer[A, ?] <:: R, Zero]  

  implicit def WriterMemberNat[R <: Effects, A]: MemberNat[Writer[A, ?], Writer[A, ?] <:: R, Zero] =
    ZeroMemberNat[Writer[A, ?], R]

  implicit def WriterMemberNatS[O[_], R <: Effects, N <: Nat, A](implicit m: MemberNat[Writer[A, ?], R, N]): MemberNat[Writer[A, ?], O <:: R, S[N]] =
    SuccessorMemberNat[Writer[A, ?], O, R, N]

  implicit def WriterMemberNatSReader[R <: Effects, N <: Nat, A, B](implicit m: MemberNat[Writer[A, ?], R, N]): MemberNat[Writer[A, ?], Reader[B, ?] <:: R, S[N]] =
    WriterMemberNatS[Reader[B, ?], R, N, A]

}
