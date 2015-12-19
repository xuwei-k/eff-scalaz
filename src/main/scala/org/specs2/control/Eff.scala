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

  trait Binder[M[_], R, A] {
    def apply[X](m: M[X]): X \/ Eff[R, A]
  }

  def interpretLoop[R <: Effects, M[_], A, B](pure: A => Eff[R, B], bind: Binder[M, R, B])(effects: Eff[M <:: R, A]): Eff[R, B] = {
    def loop(eff: Eff[M <:: R, A]): Eff[R, B] = {
      if (eff.isInstanceOf[Pure[M <:: R, A]])
         pure(eff.asInstanceOf[Pure[M <:: R, A]].value)
      else {
        val i = eff.asInstanceOf[Impure[M <:: R, A]]
        val d = decompose[M, R, A](i.union.asInstanceOf[Union[M <:: R, A]])
        if (d.toOption.isDefined)
          bind(d.toOption.get) match {
            case -\/(x) => loop(i.continuation(x))
            case \/-(b) => b
          }
        else {
          val u = d.toEither.left.toOption.get
          Impure[R, B](u.asInstanceOf[Union[R, Any]], Arrs.singleton(x => loop(i.continuation(x))))
        }
      }
    }

    loop(effects)
  }

  def interpretLoop1[R <: Effects, M[_], A, B](pure: A => B)(bind: Binder[M, R, B])(effects: Eff[M <:: R, A]): Eff[R, B] =
    interpretLoop((a: A) => EffMonad[R].point(pure(a)), bind)(effects)


  trait EffBind[M[_], R, A] {
    def apply[X](m: M[X])(continuation: X => Eff[R, A]): Eff[R, A]
  }

  /**
   * handle relay :: (a → Eff r w) →
   *                 (∀ v. t v → Arr r v w → Eff r w) →
   *                 Eff (t ’: r ) a → Eff r w
   * handle relay ret (Pure x) = ret x
   * handle relay ret h (Impure u q) = case decomp u of
   *     Right x → h x k
   *     Left u → Impure u (tsingleton k)
   *   where k = qComp q (handle relay ret h)
   *
   * This is not stack-safe we need to find better...
   *
   */
  def interpret[R <: Effects, M[_], A, B](pure: A => Eff[R, B], bind: EffBind[M, R, B])(effects: Eff[M <:: R, A]): Eff[R, B] = {
    effects match {
      case p@Pure(_) => pure(p.value)

      case i@Impure(_, _) =>
        decompose[M, R, A](i.union.asInstanceOf[Union[M <:: R, A]]) match {
          case \/-(ma) =>
            bind(ma)((a: A) => interpret(pure, bind)(i.continuation(a)))

          case -\/(u)  =>
            impure(u.asInstanceOf[Union[R, Any]], Arrs.singleton((x: Any) => interpret(pure, bind)(i.continuation.apply(x))))
      }
    }
  }

  def interpret1[R <: Effects, M[_], A, B](pure: A => B)(bind: EffBind[M, R, B])(e: Eff[M <:: R, A]): Eff[R, B] =
   interpret((a: A) => EffMonad[R].point(pure(a)), bind)(e)

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

