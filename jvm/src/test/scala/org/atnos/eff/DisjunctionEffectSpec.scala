package org.atnos.eff

import org.scalacheck.Gen.posNum
import org.specs2.{ScalaCheck, Specification}

import scalaz._, Scalaz._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class DisjunctionEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the disjunction effect monad                     $disjunctionMonad
 run the disjunction effect monad with nothing        $disjunctionWithKoMonad
 run the disjunction effect monad with reader         $disjunctionReader

 run is stack safe with Disjunction                   $stacksafeRun
 a left value can be caught and transformed to a right value $leftToRight

the left type can be modified with local     $local
"""

  def disjunctionMonad = {
    type S = DisjunctionString |: NoEffect

    val d: Eff[S, Int] =
      for {
        i <- DisjunctionEffect.right[S, String, Int](1)
        j <- DisjunctionEffect.right[S, String, Int](2)
      } yield i + j

    d.runDisjunction.run === \/.right(3)
  }

  def disjunctionWithKoMonad = {
    type S = DisjunctionString |: NoEffect

    val disjunction: Eff[S, Int] =
      for {
        i <- DisjunctionEffect.right[S, String, Int](1)
        j <- DisjunctionEffect.left[S, String, Int]("error!")
      } yield i + j

    disjunction.runDisjunction.run === \/.left("error!")
  }

  def disjunctionReader = prop { (init: Long, someValue: Int) =>

    // define a Reader / Disjunction stack
    type ReaderLong[A] = Reader[Long, A]
    type S = DisjunctionString |: ReaderLong |: NoEffect

    // create actions
    val readDisjunction: Eff[S, Int] =
      for {
        j <- DisjunctionEffect.right[S, String, Int](someValue)
        i <- ask[S, Long]
      } yield i.toInt + j

    // run effects
    readDisjunction.runDisjunction.runReader(init).run must_==
      \/.right(init.toInt + someValue)

  }.setGens(posNum[Long], posNum[Int])

  type DisjunctionString[A] = String Disjunction A

  def stacksafeRun = {
    type E = DisjunctionString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => DisjunctionEffect.right[E, String, String](i.toString))

    action.runDisjunction.run ==== \/.right(list.map(_.toString))
  }

  def leftToRight = {
    case class TooBig(value: Int)
    type D[A] = TooBig Disjunction A
    type E = D |: NoEffect

    val i = 7

    val value: Eff[E, Int] =
      if (i > 5) DisjunctionEffect.left[E, TooBig, Int](TooBig(i))
      else       DisjunctionEffect.right[E, TooBig, Int](i)

    val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
      if (k < 10) DisjunctionEffect.right[E, TooBig, Int](k)
      else        DisjunctionEffect.left[E, TooBig, Int](TooBig(k))
    }

    action.runDisjunction.run ==== \/.right(7)
  }

  def local = {
    case class Error1(m: String)

    case class Error2(e1: Error1)

    type R1 = (Error1 \/ ?) |: NoEffect
    type R2 = (Error2 \/ ?) |: NoEffect

    val action1: Eff[R1, Unit] =
      DisjunctionEffect.left(Error1("boom"))

    val action2: Eff[R2, Unit] =
      action1.localDisjunction(Error2)

    action2.runDisjunction.run ==== \/.left(Error2(Error1("boom")))
  }
}

