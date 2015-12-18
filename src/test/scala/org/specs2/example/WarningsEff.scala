package org.specs2
package example

import org.specs2.control.Eff._
import org.specs2.control.Effects._
import org.specs2.control.{Put, Effects, Eff, Member, Writer}
import scalaz.{Reader => _, Writer => _, _}, Scalaz._

object WarningsEff {

  trait WarningsTag

  type Warnings[A] = Writer[String, A] @@ WarningsTag

  /** warn the user about something that is probably wrong on his side, this is not a specs2 bug */
  def warn[R](message: String)(implicit m: Member[Warnings, R]): Eff[R, Unit] =
    Writer.tell(message)(Member.untagMember[Writer[String, ?], R, WarningsTag](m))

  /**
   * This interpreter cumulates warnings
   */
  def runWarnings[R <: Effects, A](w: Eff[Warnings <:: R, A]): Eff[R, (A, Vector[String])] = {
    val putRest = new EffBind[Warnings, R, (A, Vector[String])] {
      def apply[X](w: Warnings[X])(continuation: X => Eff[R, (A, Vector[String])]): Eff[R, (A, Vector[String])] = Tag.unwrap(w) match {
        case Put(m) => continuation(()) >>= (al => EffMonad[R].point((al._1, al._2 :+ m())))
      }
    }

    relay1((a: A) => (a, Vector[String]()))(putRest)(w)
  }

}
