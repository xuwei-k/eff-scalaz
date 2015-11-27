package org.specs2
package data

import scalaz._, Scalaz._

/**
 *
 *
 * data FFree f a where
 *   Pure :: a → FFree f a
 *   Impure :: f x → (x → FFree f a) → FFree f a
 *
 *
 *  Objective: model 4 effects
 *
 *   - IO
 *   - Read (for some configuration)
 *   - Write (for some logs)
 *   - Error (for user errors)
 *
 *
 *
 */

sealed trait FFree[F[_], A]

object FFree {

  implicit def FFreeMonad[F[_]]: Monad[FFree[F, ?]] = new Monad[FFree[F, ?]] {
    def point[A](a: =>A): FFree[F, A] =
      Pure(() => a)

    def bind[A, B](fa: FFree[F, A])(f: A => FFree[F, B]): FFree[F, B] =
      fa match {
        case Pure(run) => f(run())
        case impure: Impure[F, A] =>
          new Impure[F, B] {
            def run[X]: F[X] => X => FFree[F, B] = fx => x =>
              impure.run(fx)(x) >>= f
          }
      }

  }

}


case class Pure[F[_], A](run: () => A) extends FFree[F, A]

trait Impure[F[_], A] extends FFree[F, A] {
  def run[X]: F[X] => X => FFree[F, A]
}

/**
 * Union (r :: [∗ → ∗ ]) x
 */
case class Union[R <: Effects, A](effects: R, r: () => A)

trait Effect[F[_]] {
  def run[A]: F[A]
}


trait Effects

final case class EffectsCons[F[_], T <: Effects](head: Effect[F], tail : T) extends Effects {
  def <::[G[_]](g: Effect[G]) = EffectsCons(g, this)
}

sealed class EffectsNil extends Effects {
  def <::[G[_]](g: Effect[G]) = EffectsCons(g, this)
}

object Effects {
  type <::[H[_], T <: Effects] = EffectsCons[H, T]
  val <:: = EffectsCons
}


/**
 * class Member t r where
 * inj :: t v → Union r v
 * prj :: Union r v → Maybe (t v)
 */
trait Member[T[_], R <: Effects] {
  def inj[V](tv: T[V]): Union[R, V]
  def prj[V](u: Union[R, V]): Option[T[V]]
}

/**
 * decomp :: Union (t ’: r ) v → Either (Union r v) (t v)
 */
import Effects._
object Member {
  def decomp[H[_], T <: Effects, V](u: Union[H <:: T, V]): Union[T, V] \/ H[V] =
    ???
}
