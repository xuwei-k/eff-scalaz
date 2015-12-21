package org.specs2.control

import scalaz._, Scalaz._
import Eff._
import Member._
import Effects._

/**
 * Effect for computation which can fail
 */
object DisjunctionEffect {

  def left[R, E, A](e: E)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    impure(member.inject(-\/(e)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def right[R, E, A](a: A)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    impure(member.inject(\/-(a)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def runDisjunction[R <: Effects, E, A](r: Eff[(E \/ ?) |: R, A]): Eff[R, E \/ A] = {
    val recurse = new Recurse[(E \/ ?), R, E \/ A] {
      def apply[X](m: E \/ X) =
        m match {
          case -\/(e) => \/-(EffMonad[R].point(-\/(e)))
          case \/-(a) => -\/(a)
        }
    }

    interpret1[R, (E \/ ?), A, E \/ A]((a: A) => \/-(a))(recurse)(r)
  }

  def runDisjunctionEither[R <: Effects, E, A](r: Eff[(E \/ ?) |: R, A]): Eff[R, Either[E, A]] =
    runDisjunction(r).map(_.fold(Left.apply, Right.apply))

}

/**
 * Effect for computation which can fail and return a Throwable
 */
object DisjunctionErrorEffect {
  type Error = Throwable \/ String

  type DisjunctionError[A] = Error \/ A

  def fail[R, A](message: String)(implicit m: DisjunctionError <= R): Eff[R, A] =
    DisjunctionEffect.left[R, Error, A](\/-(message))

  def exception[R, A](t: Throwable)(implicit m: DisjunctionError <= R): Eff[R, A] =
    DisjunctionEffect.left[R, Error, A](-\/(t))


  implicit class Errored[A](c: Error \/ A) {
    def toOptionErrorMessage: Option[String] =
      c match {
        case -\/(-\/(t)) => Some(renderWithStack(t))
        case -\/(\/-(m)) => Some(m)
        case \/-(_)      => None
      }
  }

  def render(t: Throwable): String =
    s"Error[${t.getClass.getName}]" + (Option(t.getMessage) match {
      case None          => ""
      case Some(message) => s" ${message}"
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
