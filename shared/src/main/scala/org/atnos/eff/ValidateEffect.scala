package org.atnos.eff

import scalaz._, Scalaz._
import org.atnos.eff.all._
import Interpret._

/**
 * Effect for computation which can fail but will accumulate errors
 *
 * The runValidate interpreter just collects the messages and returns them at the end
 *
 */
trait ValidateEffect extends
  ValidateCreation with
  ValidateInterpretation

object ValidateEffect extends ValidateEffect

sealed trait Validate[+E, A]
case class Correct[E]() extends Validate[E, Unit]
case class Wrong[E](e: E) extends Validate[E, Unit]

trait ValidateCreation {

  /** create an Validate effect from a single Option value */
  def validateOption[R, E, A](option: Option[A], e: E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    option.map(_ => correct(())).getOrElse(wrong(e))

  /** create an Validate effect from a single \/ value */
  def validateDisjunction[R, E, A](disjunction: E \/ A)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    disjunction.fold(e => wrong(e), _ => correct(()))

  /** create a failed value */
  def wrong[R, E](e: E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    send[Validate[E, ?], R, Unit](Wrong(e))

  /** create a correct value */
  def correct[R, E, A](a: A)(implicit m: Validate[E, ?] |= R): Eff[R, A] =
    send[Validate[E, ?], R, Unit](Correct[E]()) >> Eff.EffMonad[R].point(a)

  /** check a correct condition */
  def validateCheck[R, E](condition: Boolean, e: E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    if (condition) correct(()) else wrong(e)

  /** check a correct value */
  def validateValue[R, E, A](condition: Boolean, a: A, e: E)(implicit m: Validate[E, ?] |= R): Eff[R, A] =
    if (condition) correct(a) else wrong(e) >> Eff.EffMonad[R].point(a)
}

object ValidateCreation extends ValidateCreation

trait ValidateInterpretation extends ValidateCreation {

  /** run the validate effect, yielding a ValidationNel */
  def runValidationNel[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, ValidationNel[E, A]] =
    runNel(r).map(result => Validation.fromEither(result.toEither))

  /** run the validate effect, yielding a non-empty list of failures \/ A */
  def runNel[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, NonEmptyList[E] \/ A] =
    runMap[R, U, E, NonEmptyList[E], A](r)((e: E) => NonEmptyList(e))

  /** run the validate effect, yielding a list of failures \/ A */
  def runMap[R, U, E, L : Semigroup, A](r: Eff[R, A])(map: E => L)(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, L \/ A] = {
    val recurse: StateRecurse[Validate[E, ?], A, L \/ A] = new StateRecurse[Validate[E, ?], A, L \/ A] {
      type S = Option[L]
      val init: Option[L] = None

      def apply[X](x: Validate[E, X], s: Option[L]): (X, Option[L]) =
        x match {
          case Wrong(e) => ((), s.fold(Option(map(e)))(l => Option(l |+| map(e))))
          case Correct() => ((), s)
        }

      def finalize(a: A, s: S): L \/ A =
        s.fold(\/.right[L, A](a))(\/.left[L, A])
    }

    interpretState1[R, U, Validate[E, ?], A, L \/ A]((a: A) => \/.right[L, A](a))(recurse)(r)
  }

  /** catch and handle possible wrong values */
  def catchWrong[R, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: (Validate[E, ?]) <= R): Eff[R, A] = {
    val loop = new StatelessLoop[Validate[E,?], R, A, Eff[R, A]] {
      def onPure(a: A): Eff[R, A] \/ Eff[R, A] =
        \/-(pure(a))

      def onEffect[X](m: Validate[E, X], continuation: Arrs[R, X, A]): Eff[R, A] \/ Eff[R, A] =
        m match {
          case Correct() => -\/(continuation(()))
          case Wrong(e) => -\/(handle(e))
        }
    }

    interceptStatelessLoop[R, Validate[E,?], A, A]((a: A) => pure(a), loop)(r)
  }
}

object ValidateInterpretation extends ValidateInterpretation

