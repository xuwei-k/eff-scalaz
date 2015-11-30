package org.specs2.control

import shapeless._, ops._, coproduct._
import shapeless.syntax._

import scalaz.{-\/, \/-, \/}

trait Unions[R, A]

case class CoproductUnions[R <: Coproduct, T[_], A](effects: R) extends Unions[R, A]

trait Members[M[_], R <: Coproduct] {
  def inject[A](ma: M[A])(implicit inject: Inject[R, M[A]]): Unions[R, A]

  def project[A](u: Unions[R, A])(implicit select: Selector[R, M[A]]): Option[M[A]]
}

object Members {

  implicit def CoproductUnionMembers[M[_], R <: Coproduct]: Members[M, R] = new Members[M, R] {

    def inject[A](ma: M[A])(implicit inject: Inject[R, M[A]]): Unions[R, A] =
      CoproductUnions[R, M, A](Coproduct[R](ma))

    def project[A](u: Unions[R, A])(implicit select: Selector[R, M[A]]): Option[M[A]] =
      u match {
        case CoproductUnions(effects) =>
          effects.select
      }
  }

  /**
   * Extract the first effect from a list of effects if present
   */
  def decompose[M[_], R <: Coproduct, V](u: Unions[M[V] :+: R, V])(implicit member: Members[M, M[V] :+: R]): Unions[R, V] \/ M[V] =
    member.project(u) match {
      case Some(tv) => \/-(tv)
      case None     => -\/(u.asInstanceOf[Unions[R, V]])
    }


//  implicit def EffectMembers[T[_], R <: Coproduct]: Member[T, MA :+: R] = new Member[T, MA :+: R] {
//    def inject[V](tv: T[V]): Unions[T[A] :+: R, V] =
//      CoproductUnions(Inl(tv))
//
//    def project[V](u: Union[T[A] :+: R, V]): Option[T[V]] =
//      u match {
//        case CoproductUnions(effects) => effects.select
//      }
//  }

}

//
//case class UnionNow[T[_], R <: Effects, A](ta: Option[T[A]]) extends Union[T <:: R, A]
//case class UnionNext[T[_], R <: Effects, A]() extends Union[T <:: R, A]

/**
 * Member typeclass for
 *
 * - creating a Union of effects from one single effect ("inject")
 * - extract an effect value from a list of effects (if one value has been created for that effect)
 *
 * class Member t r where
 * inj :: t v → Union r v
 * prj :: Union r v → Maybe (t v)
 */
