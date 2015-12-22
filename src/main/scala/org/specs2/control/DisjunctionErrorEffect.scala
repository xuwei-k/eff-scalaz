package org.specs2.control

import Eff._
import Effects._
import Member._

import scala.util.control.NonFatal
import scalaz._


/**
 * Effect for computation which can fail and return a Throwable, or just stop with a failure
 */
object DisjunctionErrorEffect {

  type Error = Throwable \/ String

  type DisjunctionError[A] = Error \/ Name[A]

  def ok[R, A](a: =>A)(implicit m: DisjunctionError <= R): Eff[R, A] =
    try   impure(m.inject(\/-(Name(a))), Arrs.singleton((a: A) => EffMonad[R].point(a)))
    catch { case t: Throwable => exception(t) }

  def error[R, A](error: Error)(implicit m: DisjunctionError <= R): Eff[R, A] =
    impure(m.inject(-\/(error)), Arrs.singleton((a: A) => EffMonad[R].point(a)))

  def fail[R, A](message: String)(implicit m: DisjunctionError <= R): Eff[R, A] =
    error(\/-(message))

  def exception[R, A](t: Throwable)(implicit m: DisjunctionError <= R): Eff[R, A] =
    error(-\/(t))

  def runDisjunctionError[R <: Effects, A](r: Eff[DisjunctionError |: R, A]): Eff[R, Error \/ A] = {
    val recurse = new Recurse[DisjunctionError, R, Error \/ A] {
      def apply[X](m: DisjunctionError[X]) =
        m match {
          case -\/(e) =>
            \/-(EffMonad[R].point(-\/(e)))

          case \/-(a) =>
            try -\/(a.value)
            catch { case NonFatal(t) => \/-(EffMonad[R].point(-\/(-\/(t)))) }
        }
    }

    interpret1[R, DisjunctionError, A, Error \/ A]((a: A) => \/-(a))(recurse)(r)
  }


  implicit class Errored[A](c: Error \/ A) {
    def toOptionErrorSimpleMessage: Option[String] =
      c match {
        case -\/(-\/(t)) => Some(render(t))
        case -\/(\/-(m)) => Some(m)
        case \/-(_)      => None
      }

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

