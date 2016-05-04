package org.atnos.eff

import org.scalacheck.Arbitrary._
import org.scalacheck._
import org.specs2.{ScalaCheck, Specification}
import scalaz._, Scalaz._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class EffSpec extends Specification with ScalaCheck { def is = s2"""

 run the reader monad with a pure operation $readerMonadPure
 run the reader monad with a bind operation $readerMonadBind
 run the writer monad twice                 $writerTwice

 run a reader/writer action $readerWriter

 The Eff monad is stack safe with Writer                 $stacksafeWriter
 The Eff monad is stack safe with Reader                 $stacksafeReader
 The Eff monad is stack safe with both Reader and Writer $stacksafeReaderWriter

 It is possible to run a Eff value with no effects $noEffect
 It is possible to run a Eff value with just one effect and get it back $oneEffect

"""

  def readerMonadPure = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R |: NoEffect

    ask[S, Int].runReader(initial).run === initial
  }

  def readerMonadBind = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R |: NoEffect

    val read: Eff[S, Int] =
      for {
        i <- ask[S, Int]
        j <- ask[S, Int]
      } yield i + j

    read.runReader(initial).run === initial * 2
  }

  def writerTwice = prop { _ : Int =>
    type W[A] = Writer[String, A]
    type S = W |: NoEffect

    val write: Eff[S, Unit] =
      for {
        _ <- tell("hello")
        _ <- tell("world")
      } yield ()

    write.runWriter.run ==== (((), List("hello", "world")))
  }

  def readerWriter = prop { init: Int =>

    // define a Reader / Writer stack
    type W[A] = Writer[String, A]
    type R[A] = Reader[Int, A]
    type S = W |: R |: NoEffect

    object SImplicits extends MemberImplicits
    import SImplicits._

    // create actions
    val readWrite: Eff[S, Int] =
      for {
        i <- ask[S, Int]
        _ <- tell[S, String]("init="+i)
        j <- ask[S, Int]
        _ <- tell[S, String]("result="+(i+j))
      } yield i + j

    // run effects
    readWrite.runWriter.runReader(init).run must_==
      ((init * 2, List("init="+init, "result="+(init*2))))
  }.setGen(Gen.posNum[Int])

  def stacksafeWriter = {
    type WriterString[A] = Writer[String, A]
    type E = WriterString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => WriterEffect.tell[E, String](i.toString))

    action.runWriter.run ==== ((list.as(()), list.map(_.toString)))
  }

  def stacksafeReader = {
    type ReaderString[A] = Reader[String, A]
    type E = ReaderString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverse(i => ReaderEffect.ask[E, String])

    action.runReader("h").run ==== list.as("h")
  }

  def stacksafeReaderWriter = {
    type ReaderString[A] = Reader[String, A]
    type WriterString[A] = Writer[String, A]

    type E = ReaderString |: WriterString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverse(i => ReaderEffect.ask[E, String] >>= WriterEffect.tell[E, String])

    action.runReader("h").runWriter.run ==== ((list.as(()), list.as("h")))
  }

  def noEffect =
    delay(1).runEval.run === 1

  def oneEffect =
    delay(1).detach.value === 1

  /**
   * Helpers
   */
  type F[A] = Eff[Option |: NoEffect, A]

  implicit def ArbitraryEff[R]: Arbitrary[Eff[R, Int]] = Arbitrary[Eff[R, Int]] {
    Gen.oneOf(
      Gen.choose(0, 100).map(i => EffMonad[R].point(i)),
      Gen.choose(0, 100).map(i => EffMonad[R].point(i).map(_ + 10))
    )
  }

}
