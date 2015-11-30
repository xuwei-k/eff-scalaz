package org.specs2.control

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
 */
sealed trait Eff[R, A]

case class Arrs[R, A, B](functions: Vector[Any => Eff[R, Any]]) {
  def append[C](f: B => Eff[R, C]): Arrs[R, A, C] =
    Arrs(functions :+ f.asInstanceOf[Any => Eff[R, Any]])

  def apply(a: A): Eff[R, B] =
    functions.tail.foldLeft(functions.head(a.asInstanceOf[Any])) { (res, cur) =>
      res >>= cur
    }.asInstanceOf[Eff[R, B]]
}

object Arrs {
  def singleton[R, A, B](f: A => Eff[R, B]): Arrs[R, A, B] =
    Arrs(Vector(f.asInstanceOf[Any => Eff[R, Any]]))
}

object Eff {

  implicit def EffMonad[R]: Monad[Eff[R, ?]] = new Monad[Eff[R, ?]] {
    def point[A](a: => A): Eff[R, A] =
      Pure(() => a)

    def bind[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      fa match {
        case Pure(run) => f(run())
        case Impure(union, continuation) =>
          Impure(union, continuation.append(f).asInstanceOf[Arrs[R, Any, B]])
      }
  }

  /**
   * send :: Member t r ⇒ t v → Eff r v
   * send t = Impure (inj t) (tsingleton Pure)
   */
   def send[T[_], R <: Effects, V](tv: T[V])(implicit member: Member[T, R]): Eff[R, V] =
     impure(member.inject(tv), Arrs.singleton((v: V) => EffMonad[R].point(v)))


   def pure[R, A](run: A): Eff[R, A] =
     Pure(() => run)

   def impure[R, A, X](union: Union[R, X], continuation: Arrs[R, X, A]): Eff[R, A] =
     Impure(union.asInstanceOf[Union[R, Any]], continuation.asInstanceOf[Arrs[R, Any, A]])

  def run[A](eff: Eff[EffectsNil, A]): A =
    eff match {
      case Pure(run) => run()
      case _ => sys.error("impossible")
    }

}


case class Pure[R, A](run: () => A) extends Eff[R, A]

case class Impure[R, A](union: Union[R, Any], continuation: Arrs[R, Any, A]) extends Eff[R, A]

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
trait Union[R, A]
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
trait Member[T[_], R] {
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
