package org.specs2.control

import scala.annotation.tailrec
import scalaz._
import Effects._
import Member.decompose

sealed trait Eff[R, A]

case class Pure[R, A](value: A) extends Eff[R, A]

case class Impure[R, A](union: Union[R, Any], continuation: Arrs[R, Any, A]) extends Eff[R, A]

object Eff {

  type Arr[R, A, B] = A => Eff[R, B]

  implicit def EffMonad[R]: Monad[Eff[R, ?]] = new Monad[Eff[R, ?]] {
    def point[A](a: => A): Eff[R, A] =
      Pure(a)

    def bind[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      fa match {
        case Pure(a) =>
          f(a)

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

  def pure[R, A](a: A): Eff[R, A] =
    Pure(a)

  def impure[R, A, X](union: Union[R, X], continuation: Arrs[R, X, A]): Eff[R, A] =
    Impure(union.asInstanceOf[Union[R, Any]], continuation.asInstanceOf[Arrs[R, Any, A]])

  def run[A](eff: Eff[NoEffect, A]): A =
    eff match {
      case Pure(a) => a
      case _       => sys.error("impossible")
    }

  trait Recurse[M[_], R, A] {
    def apply[X](m: M[X]): X \/ Eff[R, A]
  }

  def interpret[R <: Effects, M[_], A, B](pure: A => Eff[R, B], recurse: Recurse[M, R, B])(effects: Eff[M |: R, A]): Eff[R, B] = {
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

  def interpret1[R <: Effects, M[_], A, B](pure: A => B)(recurse: Recurse[M, R, B])(effects: Eff[M |: R, A]): Eff[R, B] =
    interpret((a: A) => EffMonad[R].point(pure(a)), recurse)(effects)

  trait StateRecurse[M[_], A, B] {
    type S
    val init: S
    def apply[X](x: M[X], s: S): (X, S)
    def finalize(a: A, s: S): B
  }

  def interpretState[R <: Effects, M[_], A, B](pure: A => Eff[R, B], recurse: StateRecurse[M, A, B])(effects: Eff[M |: R, A]): Eff[R, B] = {
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

  def interpretState1[R <: Effects, M[_], A, B](pure: A => B)(recurse: StateRecurse[M, A, B])(effects: Eff[M |: R, A]): Eff[R, B] =
    interpretState((a: A) => EffMonad[R].point(pure(a)), recurse)(effects)

  // this *really* looks like a Fold...
  trait MonadicRecurse[M[_], A, B] {
    type S
    val init: S
    def apply[X](x: M[X], s: S): (X, S) \/ B
    def finalize(a: A, s: S): B
  }

  def interpretMonadic[R <: Effects, M[_], A, B, S](pure: A => Eff[R, B], recurse: MonadicRecurse[M, A, Eff[R, B]])(effects: Eff[M |: R, A]): Eff[R, B] = {
    def loop(eff: Eff[M |: R, A], s: recurse.S): Eff[R, B] = {
      eff match {
        case Pure(a) =>
          recurse.finalize(a, s)

        case Impure(union, continuation) =>
          decompose(union) match {
            case \/-(v) =>
              recurse(v, s) match {
                case \/-(b)       => b
                case -\/((x, s1)) => loop(continuation(x), s1)
              }

            case -\/(u1) =>
              Impure[R, B](u1, Arrs.singleton(x => loop(continuation(x), s)))
          }
      }
    }

    loop(effects, recurse.init)
  }

  implicit class EffOps[R <: Effects, A](e: Eff[R, A]) {
    def into[U](implicit f: IntoPoly[R, U, A]): Eff[U, A] =
      effInto(e)(f)
  }

  def effInto[R <: Effects, U, A](e: Eff[R, A])(implicit f: IntoPoly[R, U, A]): Eff[U, A] =
    f(e)

  trait IntoPoly[R <: Effects, U, A] {
    def apply(e: Eff[R, A]): Eff[U, A]
  }

  implicit def intoNoEff[M[_], U, A](implicit m: Member[M, M |: NoEffect], mu: Member[M, U]): IntoPoly[M |: NoEffect, U, A] =
    new IntoPoly[M |: NoEffect, U, A] {
      def apply(e: Eff[M |: NoEffect, A]): Eff[U, A] = {

        e match {
          case Pure(a) =>
            EffMonad[U].point(a)

          case Impure(u, c) =>
            decompose(u) match {
              case \/-(mx) => impure[U, A, Any](mu.inject(mx), Arrs.singleton(x => effInto(c(x))))
              case -\/(u1) => sys.error("impossible")
            }
        }
      }
    }

  implicit def intoEff[M[_], R <: Effects, U, A](implicit m: Member[M, M |: R], mu: Member[M, U], recurse: IntoPoly[R, U, A]): IntoPoly[M |: R, U, A] =
    new IntoPoly[M |: R, U, A] {
      def apply(e: Eff[M |: R, A]): Eff[U, A] = {

        e match {
          case Pure(a) =>
            EffMonad[U].point(a)

          case Impure(u, c) =>
            decompose(u) match {
              case \/-(mx) => impure[U, A, Any](mu.inject(mx), Arrs.singleton(x => effInto(c(x))))
              case -\/(u1) => recurse(impure[R, A, Any](u1, c.asInstanceOf[Arrs[R, Any, A]]))
            }
        }
      }
    }
}

case class Arrs[R, A, B](functions: Vector[Any => Eff[R, Any]]) {
  def append[C](f: B => Eff[R, C]): Arrs[R, A, C] =
    Arrs(functions :+ f.asInstanceOf[Any => Eff[R, Any]])

  def apply(a: A): Eff[R, B] = {
    @tailrec
    def go(fs: Vector[Any => Eff[R, Any]], v: Any): Eff[R, B] = {
      fs match {
        case Vector(f) =>
          f(v).asInstanceOf[Eff[R, B]]

        case f +: rest =>
          f(v) match {
            case Pure(a1) => go(rest, a1)
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
  def |:[G[_]](g: Effect[G]) = EffectsCons[G, F |: T](g, this)
}

sealed class NoEffect extends Effects {
  def |:[G[_]](g: Effect[G]) = EffectsCons[G, NoEffect](g, this)
}

object Effects {
  type |:[H[_], T <: Effects] = EffectsCons[H, T]
  val |: = EffectsCons
}

