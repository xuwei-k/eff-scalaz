package org.specs2.example

import org.specs2.control.{Effects, Eff, Member, Writer}
import Effects._
import Eff._
import scalaz.{Reader => _, Writer => _, _}, Scalaz._
import Writer._

object ConsoleEff {

  trait ConsoleTag

  type Console[A] = Writer[String, A] @@ ConsoleTag

  def log[R](message: String, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) Writer.tell(message)(Member.untagMember[Writer[String, ?], R, ConsoleTag](m))
    else      EffMonad.point(())

  def logThrowable[R](t: Throwable, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) logThrowable(t)
    else      EffMonad.point(())

  def logThrowable[R](t: Throwable)(implicit m: Member[Console, R]): Eff[R, Unit] =
    log(t.getMessage, doIt = true)(m) >>
    log(t.getStackTrace.mkString("\n"), doIt = true) >>
      (if (t.getCause != null) logThrowable(t.getCause)
       else                    EffMonad.point(()))


  /**
   * This interpreter prints messages to the console
   */
  def runConsole[R <: Effects, A](w: Eff[Console <:: R, A]): Eff[R, A] =
    runConsoleToPrinter(m => println(m))(w)

  /**
   * This interpreter prints messages to a printing function
   */
  def runConsoleToPrinter[R <: Effects, A](printer: String => Unit): Eff[Console <:: R, A] => Eff[R, A] = {
    val stater = new Stater[Console, A, A, Unit] {
      val init = ()
      def apply[X](x: Console[X], s: Unit): (X, Unit) =
        Tag.unwrap(x) match {
          case p@Write(m) => (printer(m), ())
        }
      def couple(a: A, s: Unit): A = a
    }

    interpretState1((a: A) => a)(stater)
  }

}
