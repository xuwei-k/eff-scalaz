package org.specs2
package data

import scalaz._, Scalaz._
import Eff._
import Effects._
import Member._

/**
 *
 *
 * data Eff f a where
 * Pure :: a → Eff f a
 * Impure :: f x → (x → Eff f a) → Eff f a
 *
 *
 * Objective: model 4 effects
 *
 * - IO
 * - Read (for some configuration)
 * - Write (for some logs)
 * - Error (for user errors)
 *
 *
 *
 */

sealed trait Eff[R <: Effects, A]

object Eff {

  implicit def EffMonad[R <: Effects]: Monad[Eff[R, ?]] = new Monad[Eff[R, ?]] {
    def point[A](a: => A): Eff[R, A] =
      Pure(() => a)

    def bind[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      fa match {
      case Pure(run) => f(run())
      case impure: Impure[R, A] =>
        new Impure[R, B] {
          type X = impure.X
          val union: Union[R, X] = impure.union
          val continuation = (x: X) =>
            bind(impure.continuation(x))(f)
        }
      }
  }

  /**
   * send :: Member t r ⇒ t v → Eff r v
   * send t = Impure (inj t) (tsingleton Pure)
   */
   def send[T[_], R <: Effects, V](tv: T[V])(implicit member: Member[T, R]): Eff[R, V] =
     new Impure[R, V] {
       type X = V
       val union: Union[R, X] = member.inject(tv)
       val continuation = (x: X) => EffMonad[R].point(x)
     }

}


case class Pure[R <: Effects, A](run: () => A) extends Eff[R, A]

trait Impure[R <: Effects, A] extends Eff[R, A] {
  type X
  val union: Union[R, X]
  val continuation: X => Eff[R, A]
}

/**
 * EFFECTS
 */


/** one effect, basically a type constructor */
trait Effect[F[_]]

trait Effects

/**
 * Type level list of effects
 */
final case class EffectsCons[F[_], T <: Effects](head: Effect[F], tail: T) extends Effects {
  def <::[G[_]](g: Effect[G]) = EffectsCons[G, F <:: T](g, this)
}

sealed class EffectsNil extends Effects {
  def <::[G[_]](g: Effect[G]) = EffectsCons[G, EffectsNil](g, this)
}

object Effects {
  type <::[H[_], T <: Effects] = EffectsCons[H, T]
  val <:: = EffectsCons
}


/**
 * Union of type-level effects + one request
 *
 * Union (r :: [∗ → ∗ ]) x
 */
trait Union[R <: Effects, A]
case class UnionNow[T[_], R <: Effects, A](ta: Option[T[A]]) extends Union[T <:: R, A]
case class UnionNext[T[_], R <: Effects, A]() extends Union[T <:: R, A]

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
trait Member[T[_], R <: Effects] {
  def inject[V](tv: T[V]): Union[R, V]
  def project[V](u: Union[R, V]): Option[T[V]]
}

/**
 * decomp :: Union (t ’: r ) v → Either (Union r v) (t v)
 */
object Member {
  implicit def EffectMember[T[_], R <: Effects]: Member[T, T <:: R] = new Member[T, T <:: R] {
    def inject[V](tv: T[V]): Union[T <:: R, V] =
      UnionNow(Some(tv))

    def project[V](u: Union[T <:: R, V]): Option[T[V]] =
      u match {
        case UnionNow(tv) => tv
        case UnionNext()  => None
      }
  }

  /**
   * Extract the first effect from a list of effects if present
   */
  def decompose[T[_], R <: Effects, V](u: Union[T <:: R, V])(implicit member: Member[T, T <:: R]): Union[R, V] \/ T[V] =
    member.project(u) match {
      case Some(tv) => tv.right
      case None     => u.asInstanceOf[Union[R, V]].left
    }
}

/**
 * data Reader i x where
Get :: Reader i i
data Writer o x where
Put :: o → Writer o ()
Informally, we split the monolithic FReaderWriter request signature
into its components (to be combined in the open union). The
simplest Reader computation, ask of §2.1, can now be written as
ask :: Member (Reader i) r ⇒ Eff r i
ask = Impure (inj Get) return
 *
*/

trait Reader[I, X]
case class Get[I]() extends Reader[I, I]

object Reader {
  def ask[I, R <: Effects](implicit member: Member[Reader[I, ?], R]): Eff[R, I] =
    new Impure[R, I] {
      type X = I
      val union = member.inject(Get[I]())
      val continuation = (x: X) => EffMonad[R].point(x)
    }
}

trait Writer[O, X]
case class Put[O](o: O) extends Writer[O, Unit]
