package org.specs2.control

import Eff._
import Effects._
import Member._
import Interpret._

import scala.util.control.NonFatal
import scalaz._, Scalaz._

/**
 * Effect for computation which can fail and return a Throwable, or just stop with a failure
 *
 * This effect is a mix of Eval and Disjunction in the sense that every computation passed to this effect (with the ok
 * method) is considered "impure" or "faulty" by default.
 *
 * The type F is used to represent the failure type.
 *
 */
trait ErrorEffect[F] { outer =>

  /** type of errors: exceptions or failure messages */
  type Error = Throwable \/ F

  /**
   * base type for this effect: either an error or a computation to evaluate
   * scala.Name represents "by-name" value: values not yet evaluated
   */
  type ErrorOrOk[A] = Error \/ Name[A]

  /** create an Eff value from a computation */
  def ok[R, A](a: =>A)(implicit m: ErrorOrOk <= R): Eff[R, A] =
    impure(m.inject(\/-(Name(a))), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  /** create an Eff value from an error */
  def error[R, A](error: Error)(implicit m: ErrorOrOk <= R): Eff[R, A] =
    impure(m.inject(-\/(error)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  /** create an Eff value from a failure */
  def fail[R, A](failure: F)(implicit m: ErrorOrOk <= R): Eff[R, A] =
    error(\/-(failure))

  /** create an Eff value from an exception */
  def exception[R, A](t: Throwable)(implicit m: ErrorOrOk <= R): Eff[R, A] =
    error(-\/(t))

  /**
   * Run an error effect.
   *
   * Stop all computation if there is an exception or a failure.
   */
  def runError[R <: Effects, A](r: Eff[ErrorOrOk |: R, A]): Eff[R, Error \/ A] = {
    val recurse = new Recurse[ErrorOrOk, R, Error \/ A] {
      def apply[X](m: ErrorOrOk[X]) =
        m match {
          case -\/(e) =>
            \/-(EffMonad[R].point(-\/(e)))

          case \/-(a) =>
            try -\/(a.value)
            catch { case NonFatal(t) => \/-(EffMonad[R].point(-\/(-\/(t)))) }
        }
    }

    interpret1[R, ErrorOrOk, A, Error \/ A]((a: A) => \/-(a))(recurse)(r)
  }

  /**
   * OPERATIONS
   */

  implicit class ErrorEffectOps[R <: Effects, A](action: Eff[R, A]) {
    def andFinally(last: Eff[R, Unit])(implicit m: ErrorOrOk <= R): Eff[R, A] =
      outer.andFinally(action, last)

    def orElse(action2: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
      outer.orElse(action, action2)
  }

  /**
   * evaluate 2 actions possibly having error effects
   *
   * The second action must be executed whether the first is successful or not
   */
  def andFinally[R <: Effects, A](action: Eff[R, A], last: Eff[R, Unit])(implicit m: ErrorOrOk <= R): Eff[R, A] =
    action.runM(new Runner[ErrorOrOk, R, A] {
      def onPure(a: A): Eff[R, A] = last.as(a)

      def onEffect[X](mx: ErrorOrOk[X], cx: Arrs[R, X, A]): Eff[R, A] =
        try mx.fold(e => last.flatMap(_ => outer.error[R, A](e)), x => cx(x.value).andFinally(last))
        catch { case NonFatal(t) => last.flatMap(_ => outer.exception[R, A](t)) }
    })
//
//    (action, last) match {
//
//      case (Pure(e), Pure(l)) =>
//        action
//
//      case (Pure(_), Impure(u, c)) =>
//        action >>= ((a: A) => last.as(a))
//
//      case (Impure(u1, c1), Impure(u2, c2)) =>
//        (m.project(u1), m.project(u2)) match {
//          case (Some(\/-(e1)), Some(\/-(e2))) =>
//            ok {
//              try     c1(e1.value).andFinally(last)
//              catch { case NonFatal(t) => e2.value; }
//            }(m).flatMap(identity _)
//
//          case (None, Some(\/-(e2))) =>
//            last.flatMap(_ => action)
//
//          case _ =>
//            action
//        }
//
//      case _ =>
//        action
//    }

  /**
   * evaluate 2 actions possibly having error effects
   *
   * The second action must be executed if the first one is not successful
   */
  def orElse[R <: Effects, A](action1: Eff[R, A], action2: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
    action1.runM(new Runner[ErrorOrOk, R, A] {
      def onPure(a: A) = EffMonad[R].point(a)
      def onEffect[X](mx: ErrorOrOk[X], cx: Arrs[R, X, A]): Eff[R, A] =
        try mx.fold(e => action2, x => cx(x.value))
        catch { case NonFatal(_) => action2 }
    })
}

/**
 * Simple instantiation of the ErrorEffect trait with String as a Failure type
 */
object ErrorEffect extends ErrorEffect[String] {
  implicit class ErrorOrOkOps[A](c: Error \/ A) {
    def toErrorSimpleMessage: Option[String] =
      c match {
        case -\/(e) => Some(e.simpleMessage)
        case _      => None
      }

    def toErrorFullMessage: Option[String] =
      c match {
        case -\/(e) => Some(e.fullMessage)
        case _      => None
      }
  }

  implicit class ErrorOps[A](e: Error) {
    def simpleMessage: String =
      e match {
        case -\/(t) => render(t)
        case \/-(m) => m
      }

    def fullMessage: String =
      e match {
        case -\/(t) => renderWithStack(t)
        case \/-(m) => m
      }
  }


  def render(t: Throwable): String =
    s"Error[${t.getClass.getName}]" + (Option(t.getMessage) match {
      case None          => ""
      case Some(message) => s" $message"
    })

  def renderWithStack(t: Throwable): String =
    s"""============================================================
       |${render(t)}
       |------------------------------------------------------------
       |${traceWithIndent(t, "    ")}
       |============================================================
       |""".stripMargin

  def trace(t: Throwable): String =  {
    val out = new java.io.StringWriter
    t.printStackTrace(new java.io.PrintWriter(out))
    out.toString
  }

  def traceWithIndent(t: Throwable, indent: String): String =
    trace(t).lines.map(line => indent + line).mkString("\n")
}
