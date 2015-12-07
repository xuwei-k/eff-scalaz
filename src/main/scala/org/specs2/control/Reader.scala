package org.specs2.control

import org.specs2.control.MemberNat._

import scalaz._, Scalaz._
import Eff._
import Member._
import Reader._
import Effects._

sealed trait Reader[I, X]

case class Get[I]() extends Reader[I, I]

object Reader {

  def ask[R, I](implicit member: Member[Reader[I, ?], R]): Eff[R, I] =
    impure(member.inject(Get[I]()), Arrs.singleton((i: I) => EffMonad[R].point(i)))

  def runReaderOnly[R, A](initial: A)(r: Eff[R, A])(implicit member: Member[Reader[A, ?], R]): A =
    r match {
      case Pure(run) => run()

      case Impure(union, continuation) =>
        member.project(union).map(_ => runReaderOnly(initial)(continuation.apply(initial))).getOrElse(initial)
    }

  def runReader[R <: Effects, A, B](r: Eff[Reader[A, ?] <:: R, B])(initial: A): Eff[R, B] = {
    val readOne = (b: B) => EffMonad[R].point(b)

    val readRest = new EffCont[Reader[A, ?], R, B] {
      def apply[X](r: Reader[A, X])(continuation: X => Eff[R, B]): Eff[R, B] = r match {
        case Get() => continuation(initial.asInstanceOf[X])
      }
    }

    relay[R, Reader[A, ?], B, B](readOne, readRest)(r)
  }

  type ReaderStack[A, E <: Effects] = Reader[A, ?] <:: E

  implicit def ReaderMember[R <: Effects, A]: Member[Reader[A, ?], Reader[A, ?] <:: R] = 
    Member.MemberNatIsMember[Reader[A, ?], Reader[A, ?] <:: R, Zero]  

  implicit def ReaderMemberNat[R <: Effects, A]: MemberNat[Reader[A, ?], Reader[A, ?] <:: R, Zero] =
    ZeroMemberNat[Reader[A, ?], R]

  implicit def ReaderMemberNatS[O[_], R <: Effects, N <: Nat, A](implicit m: MemberNat[Reader[A, ?], R, N]): MemberNat[Reader[A, ?], O <:: R, S[N]] =
    SuccessorMemberNat[Reader[A, ?], O, R, N]

  implicit def ReaderMemberNatSWriter[R <: Effects, N <: Nat, A, B](implicit m: MemberNat[Reader[A, ?], R, N]): MemberNat[Reader[A, ?], Writer[B, ?] <:: R, S[N]] =
    ReaderMemberNatS[Writer[B, ?], R, N, A]


}
