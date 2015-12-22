package org.specs2
package control

import Eff._
import Effects._
import ReaderEffect._
import WriterEffect._
import com.ambiata.disorder.PositiveIntSmall
import org.scalacheck._, Arbitrary._
import scalaz._, Scalaz._
import EvalEffect._
import Member.{<=}
import scalacheck.ScalazProperties._

class EffSpec extends Specification with ScalaCheck { def is = s2"""

 The Eff monad respects the laws            $laws

 run the reader monad with a pure operation $readerMonadPure
 run the reader monad with a bind operation $readerMonadBind
 run the writer monad twice                 $writerTwice

 run a reader/writer action $readerWriter

 The Eff monad is stack safe with Writer                 $stacksafeWriter
 The Eff monad is stack safe with Reader                 $stacksafeReader
 The Eff monad is stack safe with both Reader and Writer $stacksafeReaderWriter

 Two stacks using different effects can be used together $together

"""

  def laws = monad.laws[F]

  def readerMonadPure = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R |: NoEffect

    implicit def ReaderStackMember: Member[R, S] =
      Member.MemberNatIsMember

    run(runReader(initial)(ask[S, Int])) === initial
  }

  def readerMonadBind = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R |: NoEffect

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
    type S = W |: NoEffect

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
    type S = W |: R |: NoEffect

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

  def stacksafeWriter = {
    type WriterString[A] = Writer[String, A]
    type E = WriterString |: NoEffect
    implicit def WriterStringMember: Member[WriterString, E] =
      Member.MemberNatIsMember

    val list = (1 to 5000).toList
    val action = list.traverseU(i => WriterEffect.tell[E, String](i.toString))

    run(WriterEffect.runWriter(action)) ==== ((list.as(()), list.map(_.toString)))
  }

  def stacksafeReader = {
    type ReaderString[A] = Reader[String, A]
    type E = ReaderString |: NoEffect
    implicit def ReaderStringMember: Member[ReaderString, E] =
      Member.MemberNatIsMember

    val list = (1 to 5000).toList
    val action = list.traverseU(i => ReaderEffect.ask[E, String])

    run(ReaderEffect.runReader("h")(action)) ==== list.as("h")
  }

  def stacksafeReaderWriter = {
    type ReaderString[A] = Reader[String, A]
    type WriterString[A] = Writer[String, A]

    type E = ReaderString |: WriterString |: NoEffect

    implicit def ReaderStringMember: Member[ReaderString, E] =
      Member.MemberNatIsMember

    implicit def WriterStringMember: Member[WriterString, E] =
      Member.MemberNatIsMember

    val list = (1 to 5000).toList
    val action = list.traverseU(i => ReaderEffect.ask[E, String] >>= WriterEffect.tell[E, String])

    run(WriterEffect.runWriter(ReaderEffect.runReader("h")(action))) ==== ((list.as(()), list.as("h")))
  }

  def together = {
    object HadoopStack {
      trait HadoopTag
      case class HadoopConf(mappers: Int)

      type HadoopReader[A] = Reader[HadoopConf, A] @@ HadoopTag
      type WriterString[A] = Writer[String, A]
      type Hadoop = HadoopReader |: WriterString |: Eval |: NoEffect

      implicit def HadoopReaderMember: Member[HadoopReader, Hadoop] =
        Member.MemberNatIsMember

      implicit def WriterStringMember: Member[WriterString, Hadoop] =
        Member.MemberNatIsMember

      def askHadoopConf[R](implicit m: HadoopReader <= R): Eff[R, HadoopConf] =
        ReaderEffect.ask(Member.untagMember[Reader[HadoopConf, ?], R, HadoopTag](m))

      def readFile(path: String): Eff[Hadoop, String] =
        for {
          c <- askHadoopConf
          _ <- tell("Reading from "+path)
        } yield c.mappers.toString

      def runHadoopReader[R <: Effects, A](conf: HadoopConf): Eff[HadoopReader |: R, A] => Eff[R, A] =
        (e: Eff[HadoopReader |: R, A]) => ReaderEffect.runTaggedReader(conf)(e)

    }

    object S3Stack {
      trait S3Tag
      case class S3Conf(bucket: String)

      type S3Reader[A] = Reader[S3Conf, A] @@ S3Tag
      type WriterString[A] = Writer[String, A]

      type S3 = S3Reader |: WriterString |: Eval |: NoEffect


      implicit def S3ReaderMember: Member[S3Reader, S3] =
        Member.MemberNatIsMember

      implicit def WriterStringMember: Member[WriterString, S3] =
        Member.MemberNatIsMember

      def askS3Conf[R](implicit m: S3Reader <= R): Eff[R, S3Conf] =
        ReaderEffect.ask(Member.untagMember[Reader[S3Conf, ?], R, S3Tag](m))

      def writeFile(key: String, content: String): Eff[S3, Unit] =
        for {
          c <- askS3Conf
          _ <- tell("Writing to bucket "+c.bucket+": "+content)
        } yield ()

      def runS3Reader[R <: Effects, A](conf: S3Conf): Eff[S3Reader |: R, A] => Eff[R, A] =
        (e: Eff[S3Reader |: R, A]) => ReaderEffect.runTaggedReader(conf)(e)
    }

    import HadoopStack._
    import S3Stack.{WriterString=>_,_}

    type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect

    implicit class Into[R, A](e: Eff[R, A]) {
      def into[U]: Eff[U, A] = ???
    }


    val action: Eff[HadoopS3, Unit] = for {
      s <- readFile("/tmp/data").into[HadoopS3]
      _ <- writeFile("key", s)  .into[HadoopS3]
    } yield ()

    (action |> runS3Reader(S3Conf("bucket")) |> runHadoopReader(HadoopConf(10)) |> runWriter |> runEval |> run) ====
      (((), List("something")))
  }

  /**
   * Helpers
   */
   type F[A] = Eff[NoEffect, A]

   implicit def EffEqual[A]: Equal[F[A]] = new Equal[F[A]] {
     def equal(a1: F[A], a2: F[A]): Boolean =
       run(a1) == run(a2)
   }

   implicit def ArbitraryEff: Arbitrary[F[Int]] = Arbitrary[F[Int]] {
     Gen.oneOf(
       Gen.choose(0, 100).map(i => EffMonad[NoEffect].point(i)),
       Gen.choose(0, 100).map(i => EffMonad[NoEffect].point(i).map(_ + 10))
     )
   }

   implicit def ArbitraryEffFunction: Arbitrary[F[Int => Int]] =
     Arbitrary(arbitrary[Int => Int].map(f => EffMonad[NoEffect].point(f)))

}
