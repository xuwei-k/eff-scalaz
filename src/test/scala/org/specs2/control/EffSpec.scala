package org.specs2
package control

import Eff._
import Effects._
import Reader._
import Writer._
import com.ambiata.disorder.PositiveIntSmall
import scalaz._, Scalaz._

class EffSpec extends Specification with ScalaCheck { def is = s2"""

 run the reader monad with a pure operation $readerMonadPure
 run the reader monad with a bind operation $readerMonadBind
 run the writer monad twice                 $writerTwice

 run a reader/writer action $readerWriter

"""

  def readerMonadPure = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R <:: EffectsNil

    implicit def ReaderStackMember: Member[R, S] =
      Member.MemberNatIsMember

    run(runReader(initial)(ask[S, Int])) === initial
  }

  def readerMonadBind = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R <:: EffectsNil

    implicit def ReaderStackMember: Member[R, S] =
      Member.MemberNatIsMember

    val read: Eff[S, Int] =
      for {
        i <- ask[S, Int]
        j <- ask[S, Int]
      } yield i + j

    run(runReader(initial)(read)) === initial * 2
  }

  def writerTwice = prop { _ : Int =>
    type W[A] = Writer[String, A]
    type S = W <:: EffectsNil

    implicit def WriterStackMember: Member[W, S] =
      Member.MemberNatIsMember

    val write: Eff[S, Unit] =
      for {
        _ <- tell[S, String]("hello")
        _ <- tell[S, String]("world")
      } yield ()

    run(runWriter(write)) === ((), List("hello", "world"))
  }

  def readerWriter = prop { init: PositiveIntSmall =>

    // define a Reader / Writer stack
    type W[A] = Writer[String, A]
    type R[A] = Reader[Int, A]
    type S = W <:: R <:: EffectsNil

    implicit def ReaderStackMember: Member[R, S] =
      Member.MemberNatIsMember

    implicit def WriterStack: Member[W, S] =
      Member.MemberNatIsMember

    // create actions
    val readWrite: Eff[S, Int] =
      for {
        i <- ask[S, Int]
        _ <- tell[S, String]("initial="+i)
        j <- ask[S, Int]
        _ <- tell[S, String]("result="+(i+j))
      } yield i + j

    // run effects
    val initial = init.value
    run(runReader(initial)(runWriter(readWrite))) must_==
      (initial * 2, List("initial="+initial, "result="+(initial*2)))
  }

}
