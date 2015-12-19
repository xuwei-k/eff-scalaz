package org.specs2.control

import scalaz._, Scalaz._
import Effects._

/**
 * Union of type-level effects + one request
 *
 * Union (r :: [∗ → ∗ ]) x
 *
 * data Union (r :: [ * -> * ]) v where
 * UNow  :: t v -> Union (t ': r) v
 * UNext :: Union r v -> Union (any ': r) v
 *
 *
 */
trait Union[R, A]

object Union {
  def now[T[_], R <: Effects, A](ta: T[A]): Union[T <:: R, A] =
    UnionNow(ta)

  def next[T[_], O[_], R <: Effects, A](u: Union[R, A]): Union[O <:: R, A] =
    UnionNext(u)
}

case class UnionNow[T[_], R <: Effects, A](ta: T[A]) extends Union[T <:: R, A]

case class UnionNext[T[_], O[_], R <: Effects, A](u: Union[R, A]) extends Union[O <:: R, A]

// data P (n::Nat) = P
case class P[N <: Nat]()

object P {

  implicit def ZeroPredicate: P[Zero] =
    P[Zero]

  implicit def SuccPredicate[N <: Nat](implicit prev: P[N]): P[Succ[N]] =
    P[Succ[N]]
}

/**
 * class Member' t r (n :: Nat) where
 * inj' :: P n -> t v -> Union r v
 * prj' :: P n -> Union r v -> Maybe (t v)
 *
 * Inject a given effect at a given rank
 * Project a given effect from a given rank
 *
 * The rank is modelled as a type-level natural
 * and modelled by a value of that type
 */
trait MemberNat[T[_], R <: Effects, N <: Nat] {
  def inject[V](rank: P[N], effect: T[V]): Union[R, V]

  def project[V](rank: P[N], union: Union[R, V]): Option[T[V]]
}

object MemberNat {
  /**
   * instance (r ~ (t ': r')) => Member' t r Z where
   * inj' _ = UNow
   * prj' _ (UNow x) = Just x
   * prj' _ _        = Nothing
   *
   * Injection and projection for rank 0
   *
   * The idea is that:
   *
   *  - if T is an effect
   *  - then it is the first effect of T <:: R
   */
  implicit def ZeroMemberNat[T[_], R <: Effects]: MemberNat[T, T <:: R, Zero] = new MemberNat[T, T <:: R, Zero] {
    def inject[V](rank: P[Zero], effect: T[V]): Union[T <:: R, V] =
      Union.now(effect)

    def project[V](predicate: P[Zero], union: Union[T <:: R, V]): Option[T[V]] =
      union match {
        case UnionNow(x) => Some(x)
        case _ => None
      }
  }

  /**
   * instance (r ~ (t' ': r'), Member' t r' n) => Member' t r (S n) where
   * inj' _ = UNext . inj' (P::P n)
   * prj' _ (UNow _)  = Nothing
   * prj' _ (UNext x) = prj' (P::P n) x
   *
   * Injection and projection for rank S(N)
   *
   * r' == R
   * r  == O <:: R
   * t  == T
   * t' == O
   *
   * The idea is that:
   *
   *  - if T is one effect of R for a given N
   *  - then T is one effect of O <:: R for S[N]
   *
   */
  implicit def SuccessorMemberNat[T[_], O[_], R <: Effects, N <: Nat](implicit m: MemberNat[T, R, N]): MemberNat[T, O <:: R, Succ[N]] = new MemberNat[T, O <:: R, Succ[N]] {
    def inject[V](predicate: P[Succ[N]], effect: T[V]) =
      Union.next(m.inject[V](P[N](), effect))

    def project[V](predicate: P[Succ[N]], union: Union[O <:: R, V]) =
      union match {
        case UnionNow(_) => None
        case UnionNext(u) => m.project[V](P[N](), u)
      }
  }

}

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
 */
object Member {

  /**
   * class (Member' t r (FindElem t r)) => Member t r where
   * inj :: t v -> Union r v
   * prj :: Union r v -> Maybe (t v)
   *
   * instance (Member' t r (FindElem t r)) => Member t r where
   * inj = inj' (P::P (FindElem t r))
   * prj = prj' (P::P (FindElem t r))
   */
  implicit def MemberNatIsMember[T[_], R <: Effects, N <: Nat](implicit m: MemberNat[T, R, N], p: P[N]): Member[T, R] = new Member[T, R] {
    def inject[V](tv: T[V]): Union[R, V] =
      m.inject(p, tv)

    def project[V](u: Union[R, V]): Option[T[V]] =
      m.project(p, u)
  }

  /**
   * decomp :: Union (t ’: r ) v → Either (Union r v) (t v)
   *
   * Extract the first effect from a list of effects if present
   */
  def decompose[T[_], R <: Effects, V](u: Union[T <:: R, V]): Union[R, V] \/ T[V] =
    u match {
      case UnionNow(tv)     => tv.right
      case UnionNext(union) => union.left
    }


  def untagMember[T[_], R, TT](m: Member[({type X[A]=T[A] @@ TT})#X, R]): Member[T, R] =
    new Member[T, R] {
      def inject[V](tv: T[V]): Union[R, V] =
        m.inject(Tag(tv))

      def project[V](u: Union[R, V]): Option[T[V]] =
        m.project(u).map(Tag.unwrap)
    }

  type <=[M[_], R] = Member[M, R]

}


/**
 * data Nat = Z | S Nat
 */
sealed trait Nat

trait Zero extends Nat

trait Succ[N <: Nat] extends Nat
