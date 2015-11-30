package org.specs2.control

import shapeless._, ops._, coproduct._
import shapeless.syntax._

import scalaz.{-\/, \/-, \/}

trait Unions[R, A]

case class CoproductUnions[R <: Coproduct, T[_], A](effects: R) extends Unions[R, A]

trait Members[M[_], R, A] {
  def inject(ma: M[A]): Unions[R, A]

  def project(u: Unions[R, A]): Option[M[A]]
}

object Members {

  implicit def CoproductUnionMembers[M[_], R <: Coproduct, A](implicit injector: Inject[R, M[A]],
    selector: Selector[R, M[A]]): Members[M, R, A] = new Members[M, R, A] {

    def inject(ma: M[A]): Unions[R, A] =
      CoproductUnions[R, M, A](Coproduct(ma))

    def project(u: Unions[R, A]): Option[M[A]] =
      u match {
      case CoproductUnions(effects) =>
        effects.select
      }
  }

  /**
   * Extract the first effect from a list of effects if present
   */
  def decompose[M[_], R <: Coproduct, V](u: Unions[M[V] :+: R, V])(implicit member: Members[M, M[V] :+: R, V]): Unions[R, V] \/ M[V] =
    member.project(u) match {
      case Some(tv) => \/-(tv)
      case None     => -\/(u.asInstanceOf[Unions[R, V]])
    }


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
