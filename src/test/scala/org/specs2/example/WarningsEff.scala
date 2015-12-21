package org.specs2
package example

import org.specs2.control.Eff._
import org.specs2.control.Effects._
import org.specs2.control.{Effects, Eff, Member, Writer}
import Writer._
import scalaz.{Reader => _, Writer => _, _}

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
    val recurse = new StateRecurse[Warnings, A, (A, Vector[String])] {
      type S = Vector[String]
      val init = Vector()

      def apply[X](x: Warnings[X], s: Vector[String]): (X, Vector[String]) =
        Tag.unwrap(x) match {
          case p@Write(_) => (().asInstanceOf[X], s :+ p.value)
        }

      def finalize(a: A, s: Vector[String]): (A, Vector[String]) =
        (a, s)
    }

    interpretState1[R, Warnings, A, (A, Vector[String])]((a: A) => (a, Vector()))(recurse)(w)
  }

}
