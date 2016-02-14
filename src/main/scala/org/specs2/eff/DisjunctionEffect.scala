package org.specs2.eff

import scalaz._
import scalaz.syntax.functor._
import Eff._
import Interpret._
import Effects.|:

/**
 * Effect for computation which can fail
 */
object DisjunctionEffect extends
  DisjunctionCreation with
  DisjunctionInterpretation with
  DisjunctionImplicits

trait DisjunctionCreation {

  /** create a Disjunction effect from a single Option value */
  def fromOption[R, E, A](option: Option[A], e: E)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    option.fold[Eff[R, A]](left[R, E, A](e))(right[R, E, A])

  /** create a Disjunction effect from a single \/ value */
  def fromDisjunction[R, E, A](xor: E \/ A)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    xor.fold[Eff[R, A]](left[R, E, A], right[R, E, A])

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    send[E \/ ?, R, A](-\/(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    send[E \/ ?, R, A](\/-(a))
}

trait DisjunctionInterpretation {
  /** run the disjunction effect, yielding E \/ A */
  def runDisjunction[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E \/ ?), R, U]): Eff[U, E \/ A] = {
    val recurse = new Recurse[(E \/ ?), U, E \/ A] {
      def apply[X](m: E \/ X) =
        m match {
          case -\/(e) => \/-(EffMonad[U].pure(-\/(e)))
          case \/-(a) => -\/(a)
        }
    }

    interpret1[R, U, (E \/ ?), A, E \/ A]((a: A) => \/-(a): E \/ A)(recurse)(r)
  }

  /** run the disjunction effect, yielding Either[E, A] */
  def runDisjunctionEither[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E \/ ?), R, U]): Eff[U, Either[E, A]] =
    runDisjunction(r).map(_.fold(util.Left.apply, util.Right.apply))

}

trait DisjunctionImplicits extends DisjunctionImplicitsLower {
  implicit def DisjunctionMemberZero[R, A]: Member.Aux[\/[A, ?], \/[A, ?] |: NoEffect, NoEffect] = {
    type T[X] = \/[A, X]
    Member.zero[T]
  }

  implicit def DisjunctionMemberFirst[R <: Effects, A]: Member.Aux[\/[A, ?], \/[A, ?] |: R, R] = {
    type T[X] = \/[A, X]
    Member.first[T, R]
  }
}

trait DisjunctionImplicitsLower {
  implicit def DisjunctionMemberSuccessor[O[_], R <: Effects, U <: Effects, A](implicit m: Member.Aux[\/[A, ?], R, U]): Member.Aux[\/[A, ?], O |: R, O |: U] = {
    type T[X] = \/[A, X]
    Member.successor[T, O, R, U]
  }
}
object DisjunctionImplicits extends DisjunctionImplicits
