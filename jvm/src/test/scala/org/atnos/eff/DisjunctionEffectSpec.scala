package org.atnos.eff

import org.scalacheck.Gen.posNum
import org.specs2.{ScalaCheck, Specification}

import scalaz._, Scalaz._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.scalacheck.Gen

class DisjunctionEffectSpec extends Specification with ScalaCheck { def is = s2"""

 an disjunction value can be injected in the stack    $disjunctionCreation

 run the disjunction effect monad                     $disjunctionMonad
 run the disjunction effect monad with nothing        $disjunctionWithKoMonad
 run the disjunction effect monad with reader         $disjunctionReader

 run is stack safe with disjunction                   $stacksafeRun
 a left value can be caught and transformed to a right value $leftToRight

 the left type can be modified with local in a different stack $local
 the left type can be run with local in the same stack  $localRun

"""

  def disjunctionCreation = prop { stringOrInt: String Either Int =>
    type S = Fx.fx1[DisjunctionString]

    val disjunction = \/.fromEither(stringOrInt)
    val e: Eff[S, Int] = fromDisjunction(disjunction)
    e.runEither.run ==== disjunction.toEither
  }

  def disjunctionMonad = {
    type S = Fx.fx1[DisjunctionString]

    val disjunction: Eff[S, Int] =
      for {
        i <- DisjunctionEffect.right[S, String, Int](1)
        j <- DisjunctionEffect.right[S, String, Int](2)
      } yield i + j

    disjunction.runDisjunction.run === \/-(3)
  }

  def disjunctionWithKoMonad = {
    type S = Fx.fx1[DisjunctionString]

    val disjunction: Eff[S, Int] =
      for {
        i <- DisjunctionEffect.right[S, String, Int](1)
        j <- DisjunctionEffect.left[S, String, Int]("error!")
      } yield i + j

    disjunction.runDisjunction.run === -\/("error!")
  }

  def disjunctionReader = prop { (init: Long, someValue: Int) =>

    // define a Reader / \/ stack
    type ReaderLong[A] = Reader[Long, A]
    type S = Fx.fx2[DisjunctionString, ReaderLong]

    // create actions
    val readDisjunction: Eff[S, Int] =
      for {
        j <- DisjunctionEffect.right[S, String, Int](someValue)
        i <- ask[S, Long]
      } yield i.toInt + j

    // run effects
    readDisjunction.runDisjunction.runReader(init).run must_==
      \/-(init.toInt + someValue)

  }.setGens(posNum[Long], posNum[Int])

  type DisjunctionString[A] = String \/ A

  def stacksafeRun = {
    type E = Fx.fx1[DisjunctionString]

    val list = (1 to 5000).toList
    val action = list.traverseU(i => DisjunctionEffect.right[E, String, String](i.toString))

    action.runDisjunction.run ==== \/-(list.map(_.toString))
  }

  def leftToRight = prop { i: Int =>
    case class TooBig(value: Int)
    type D[A] = TooBig \/ A
    type E = Fx.fx1[D]

    val i = 7

    val value: Eff[E, Int] =
      if (i > 5) DisjunctionEffect.left[E, TooBig, Int](TooBig(i))
      else       DisjunctionEffect.right[E, TooBig, Int](i)

    val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
      if (k < 10) DisjunctionEffect.right[E, TooBig, Int](k)
      else        DisjunctionEffect.left[E, TooBig, Int](TooBig(k))
    }

    val expected: TooBig \/ Int =
      if (i < 10) \/.right(i) else \/.left(TooBig(i))

    val actual: TooBig \/ Int = action.runDisjunction.run

    actual == expected

  }.setGen(Gen.oneOf(14, 12))

  def local = {
    case class Error1(m: String)

    case class Error2(e1: Error1)

    type R1 = Fx.fx1[(Error1 \/ ?)]
    type R2 = Fx.fx1[(Error2 \/ ?)]

    val action1: Eff[R1, Unit] =
      DisjunctionEffect.left(Error1("boom"))

    val action2: Eff[R2, Unit] =
      action1.localDisjunction(Error2)

    action2.runDisjunction.run ==== \/.left(Error2(Error1("boom")))
  }

  def localRun = {
    case class Error1(m: String)

    case class Error2(e1: Error1)

    type R1 = Fx.fx2[Error1 \/ ?, Error2 \/ ?]

    val action1: Eff[R1, Unit] =
      DisjunctionEffect.left(Error1("boom"))

    action1.runLocalDisjunction(Error2).runDisjunction.run ==== \/.left(Error2(Error1("boom")))
  }
}

