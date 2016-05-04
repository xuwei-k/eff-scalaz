// 8<---
package org.atnos.site.snippets

import scalaz.\/
import scalaz.-\/
import org.atnos.eff._, all._
import org.atnos.eff.interpret._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

trait FutureEffectSnippet {

// 8<---
import scala.concurrent.ExecutionContext.Implicits.global

object FutureEffect {
  type Fut[A] = Future[() => A]

  def future[R, A](a: => A)(implicit m: Fut <= R): Eff[R, A] =
    send[Fut, R, A](Future(() => a))

  def runFuture[R <: Effects, U <: Effects, A, B](atMost: Duration)(effects: Eff[R, A])(
     implicit m: Member.Aux[Fut, R, U]): Eff[U, A] = {

    val recurse = new Recurse[Fut, U, A] {
      def apply[X](m: Fut[X]): X \/ Eff[U, A] =
        -\/(Await.result(m.map(_ ()), atMost))
    }
    interpret1((a: A) => a)(recurse)(effects)(m)
  }
}

// 8<---
}

object FutureEffectSnippet extends FutureEffectSnippet

