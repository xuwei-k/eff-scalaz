package org.specs2.control


import scalaz._, Scalaz._
import Eff._
import Member._
import MemberNat._
import Optional._
import Effects._

sealed trait Optional[A]

case class Nothing[A]() extends Optional[A]
case class Something[A](a: A) extends Optional[A]

object Optional {

  def nothing[R, A](implicit member: Member[Optional[?], R]): Eff[R, A] =
    impure(member.inject(Nothing[A]()), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def something[R, A](a: A)(implicit member: Member[Optional[?], R]): Eff[R, A] =
    impure(member.inject(Something[A](a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def runOptional[R <: Effects, A](r: Eff[Optional[?] <:: R, A]): Eff[R, Option[A]] = {
    val runPure = (a: A) => EffMonad[R].point(Option(a))

    val runImpure = new EffCont[Optional, R, Option[A]] {
      def apply[X](r: Optional[X])(continuation: X => Eff[R, Option[A]]): Eff[R, Option[A]] = r match {
        case Nothing()   => pure(None)
        case Something(a) => continuation(a) >>= ((a: Option[A]) => EffMonad.point(a))
      }
    }

    relay[R, Optional, A, Option[A]](runPure, runImpure)(r)
  }

  type OptionalStack[E <: Effects] = Optional[?] <:: E

  implicit def OptionalMember[R <: Effects]: Member[Optional, Optional <:: R] = 
    Member.MemberNatIsMember[Optional, Optional <:: R, Zero](OptionalMemberNat, P[Zero])  

  implicit def OptionalMemberNat[R <: Effects, A]: MemberNat[Optional, Optional <:: R, Zero] =
    ZeroMemberNat[Optional, R]

  implicit def OptionalMemberNatS[O[_], R <: Effects, N <: Nat, A](implicit m: MemberNat[Optional, R, N]): MemberNat[Optional, O <:: R, S[N]] =
    SuccessorMemberNat[Optional, O, R, N]

}

