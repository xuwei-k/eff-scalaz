package org.specs2.control

import scala.annotation.tailrec
import scalaz._
import Effects._
import Member._

/**
 * data Eff f a where
 *   Pure :: a → Eff f a
 *   Impure :: f x → (x → Eff f a) → Eff f a
 *
 */
sealed trait Eff[R, A]

case class Pure[R, A](private val run: () => A) extends Eff[R, A] {
  def value: A =
    run()
}
case class Impure[R, A](union: Union[R, Any], continuation: Arrs[R, Any, A]) extends Eff[R, A]

object Eff {

  type Arr[R, A, B] = A => Eff[R, B]

  implicit def EffMonad[R]: Monad[Eff[R, ?]] = new Monad[Eff[R, ?]] {
    def point[A](a: => A): Eff[R, A] =
      Pure(() => a)

    def bind[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      fa match {
        case p@Pure(_) => f(p.value)
        case Impure(union, continuation) =>
          Impure(union, continuation.append(f))
      }
  }

  /**
   * send :: Member t r ⇒ t v → Eff r v
   * send t = Impure (inj t) (tsingleton Pure)
   */
  def send[T[_], R, V](tv: T[V])(implicit member: Member[T, R]): Eff[R, V] =
    impure(member.inject(tv), Arrs.singleton((v: V) => EffMonad[R].point(v)))

  def unit[R]: Eff[R, Unit] =
    EffMonad.point(())

  def pure[R, A](run: =>A): Eff[R, A] =
    Pure(() => run)

  def impure[R, A, X](union: Union[R, X], continuation: Arrs[R, X, A]): Eff[R, A] =
    Impure(union.asInstanceOf[Union[R, Any]], continuation.asInstanceOf[Arrs[R, Any, A]])

  def run[A](eff: Eff[EffectsNil, A]): A =
    eff match {
      case p@Pure(_) => p.value
      case _         => sys.error("impossible")
    }

  trait Recurse[M[_], R, A] {
    def apply[X](m: M[X]): X \/ Eff[R, A]
  }

  def interpret[R <: Effects, M[_], A, B](pure: A => Eff[R, B], recurse: Recurse[M, R, B])(effects: Eff[M <:: R, A]): Eff[R, B] = {
    val recurseState = new MonadicRecurse[M, A, Eff[R, B]] {
      type S = Unit
      val init = ()
      def apply[X](x: M[X], s: Unit): (X, Unit) \/ Eff[R, B] =
        recurse(x).leftMap((_, ()))

      def finalize(a: A, s: Unit): Eff[R, B] =
        pure(a)
    }
    interpretMonadic(pure, recurseState)(effects)
  }

  def interpret1[R <: Effects, M[_], A, B](pure: A => B)(recurse: Recurse[M, R, B])(effects: Eff[M <:: R, A]): Eff[R, B] =
    interpret((a: A) => EffMonad[R].point(pure(a)), recurse)(effects)

  trait StateRecurse[M[_], A, B] {
    type S
    val init: S
    def apply[X](x: M[X], s: S): (X, S)
    def finalize(a: A, s: S): B
  }

  def interpretState[R <: Effects, M[_], A, B](pure: A => Eff[R, B], recurse: StateRecurse[M, A, B])(effects: Eff[M <:: R, A]): Eff[R, B] = {
    val recurseExit = new MonadicRecurse[M, A, Eff[R, B]] {
      type S = recurse.S
      val init: S = recurse.init

      def apply[X](x: M[X], s: S): (X, S) \/ Eff[R, B] =
        -\/(recurse(x, s))

      def finalize(a: A, s: S): Eff[R, B] =
        EffMonad[R].point(recurse.finalize(a, s))
    }
    interpretMonadic(pure, recurseExit)(effects)
  }

  def interpretState1[R <: Effects, M[_], A, B](pure: A => B)(recurse: StateRecurse[M, A, B])(effects: Eff[M <:: R, A]): Eff[R, B] =
    interpretState((a: A) => EffMonad[R].point(pure(a)), recurse)(effects)

  // this *really* looks like a Fold...
  trait MonadicRecurse[M[_], A, B] {
    type S
    val init: S
    def apply[X](x: M[X], s: S): (X, S) \/ B
    def finalize(a: A, s: S): B
  }

  def interpretMonadic[R <: Effects, M[_], A, B, S](pure: A => Eff[R, B], recurse: MonadicRecurse[M, A, Eff[R, B]])(effects: Eff[M <:: R, A]): Eff[R, B] = {
    def loop(eff: Eff[M <:: R, A], s: recurse.S): Eff[R, B] = {
      eff match {
        case p @ Pure(_)   => recurse.finalize(p.value, s)
        case i @ Impure(_,_) =>
          decompose[M, R, A](i.union.asInstanceOf[Union[M <:: R, A]]) match {
            case \/-(v) =>
              recurse(v, s) match {
                case \/-(b)       => b
                case -\/((x, s1)) => loop(i.continuation(x), s1)
              }

            case -\/(u) =>
              Impure[R, B](u.asInstanceOf[Union[R, Any]], Arrs.singleton(x => loop(i.continuation(x), s)))
          }
      }
    }

    loop(effects, recurse.init)
  }


}

case class Arrs[R, A, B] private(functions: Vector[Any => Eff[R, Any]]) {
  def append[C](f: B => Eff[R, C]): Arrs[R, A, C] =
    Arrs(functions :+ f.asInstanceOf[Any => Eff[R, Any]])

  /**
   * qApp :: Arrs r b w → b → Eff r w
   * qApp q x = case tviewl q of
   *   TOne k → k x
   *   k : | t → bind’ (k x) t
   *   where bind’ :: Eff r a → Arrs r a b → Eff r b
   *         bind’ (Pure y) k = qApp k y
   * bind’ (Impure u q) k = Impure u (q >< k)
   */
  def apply(a: A): Eff[R, B] = {
    @tailrec
    def go(fs: Vector[Any => Eff[R, Any]], v: Any): Eff[R, B] = {
      fs match {
        case Vector(f) =>
          f(v).asInstanceOf[Eff[R, B]]

        case f +: rest =>
          f(v) match {
            case p: Pure[_,_] => go(rest, p.value)
            case Impure(u, q) => Impure[R, B](u, q.copy(functions = q.functions ++ rest))
          }
      }
    }

    go(functions, a)
  }
}

object Arrs {
  def singleton[R, A, B](f: A => Eff[R, B]): Arrs[R, A, B] =
    Arrs(Vector(f.asInstanceOf[Any => Eff[R, Any]]))
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

