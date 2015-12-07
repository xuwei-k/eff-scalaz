package org.specs2.control

import scalaz._, Scalaz._
import Eff._
import Effects._
import Member._

/**
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
        case Pure(run) => f(run())
        case Impure(union, continuation) =>
          Impure(union, continuation.append(f))
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

  trait EffCont[M[_], R, A] {
    def apply[X]: M[X] => (X => Eff[R, A]) => Eff[R, A]
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
   */
  def relay[R <: Effects, M[_], A, B](ret: A => Eff[R, B], cont: EffCont[M, R, B])(effects: Eff[M <:: R, A]): Eff[R, B] =
    effects match {
      case Pure(a) => ret(a())
      case Impure(union, continuation) =>
        decompose[M, R, Any](union.asInstanceOf[Union[M <:: R, Any]]) match {
          case \/-(mx) => cont.apply(mx)(qComp(continuation, relay(ret, cont)))
          case -\/(u)  => impure(u.asInstanceOf[Union[R, Any]], Arrs.singleton((x: Any) => relay(ret, cont)(continuation.apply(x))))
        }
    }

  /**
   * qComp :: Arrs r a b → (Eff r b → Eff r’ c) → Arr r’ a c
   * qComp g h = h ◦ qApp g
   */
  private def qComp[R1, R2, A, B, C](arrs: Arrs[R1, A, B], f: Eff[R1, B] => Eff[R2, C]): Arr[R2, A, C] = 
    (a: A) => f(arrs(a))   
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
  def apply(a: A): Eff[R, B] =
    functions match {
      case Vector(f) => f(a).asInstanceOf[Eff[R, B]]  
      case f +: rest => 
        f(a) match {
          case p: Pure[_,_] => Arrs(rest)(p.value)  
          case Impure(u, q) => Impure[R, B](u, q.copy(functions = q.functions ++ rest))
        }  
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

