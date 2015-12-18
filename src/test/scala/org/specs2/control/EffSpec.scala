package org.specs2
package control

import Eff._
import Effects._
import Reader._
import Writer._
import Eval.runEval
import com.ambiata.disorder.PositiveIntSmall
import org.scalacheck._, Arbitrary._
import scalaz._, Scalaz._
import scalacheck.ScalazProperties._

class EffSpec extends Specification with ScalaCheck { def is = s2"""

 The Eff monad respects the laws            $laws

 run the reader monad with a pure operation $readerMonadPure
 run the reader monad with a bind operation $readerMonadBind
 run the writer monad twice                 $writerTwice

 run a reader/writer action $readerWriter

 The Eff monad is stack safe $stackSafe

"""

  def laws = monad.laws[F]

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

    run(runWriter(write)) ==== (((), List("hello", "world")))
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
      ((initial * 2, List("initial="+initial, "result="+(initial*2))))
  }

  def stackSafe = {
    type E = Eval <:: EffectsNil

    val list = (1 to 5000).toList
    val action = list.traverseU(i => Eval.delay[E, Int](i))

    run(runEval(action)) ==== list
  } //.pendingUntilFixed

  /**
   * Helpers
   */
   type F[A] = Eff[EffectsNil, A]

   implicit def EffEqual[A]: Equal[F[A]] = new Equal[F[A]] {
     def equal(a1: F[A], a2: F[A]): Boolean =
       run(a1) == run(a2)
   }

   implicit def ArbitraryEff: Arbitrary[F[Int]] = Arbitrary[F[Int]] {
     Gen.oneOf(
       Gen.choose(0, 100).map(i => EffMonad[EffectsNil].point(i)),
       Gen.choose(0, 100).map(i => EffMonad[EffectsNil].point(i).map(_ + 10))
     )
   }

   implicit def ArbitraryEffFunction: Arbitrary[F[Int => Int]] =
     Arbitrary(arbitrary[Int => Int].map(f => EffMonad[EffectsNil].point(f)))

}
