package org.specs2.eff

import com.ambiata.disorder.{PositiveIntSmall, PositiveLongSmall}
import DisjunctionEffect._
import ReaderEffect._
import Eff._
import Effects._
import org.specs2.{ScalaCheck, Specification}

import scalaz._, Scalaz._

class DisjunctionEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the disjunction monad                     $disjunctionMonad
 run the disjunction monad with nothing        $disjunctionWithKoMonad
 run the disjunction monad with reader         $disjunctionReader

 run is stack safe with Disjunction      $stacksafeRun

"""

  def disjunctionMonad = {
    type S = DisjunctionString |: NoEffect

    val disjunction: Eff[S, Int] =
      for {
        i <- DisjunctionEffect.right[S, String, Int](1)
        j <- DisjunctionEffect.right[S, String, Int](2)
      } yield i + j

    run(runDisjunction(disjunction)) === \/-(3)
  }

  def disjunctionWithKoMonad = {
    type S = DisjunctionString |: NoEffect

    val disjunction: Eff[S, Int] =
      for {
        i <- DisjunctionEffect.right[S, String, Int](1)
        j <- DisjunctionEffect.left[S, String, Int]("error!")
      } yield i + j

    run(runDisjunction(disjunction)) === -\/("error!")
  }

  def disjunctionReader = prop { (init: PositiveLongSmall, someValue: PositiveIntSmall) =>

    // define a Reader / Disjunction stack
    type ReaderLong[A] = Reader[Long, A]
    type S = DisjunctionString |: ReaderLong |: NoEffect

    // create actions
    val readDisjunction: Eff[S, Int] =
      for {
        j <- DisjunctionEffect.right[S, String, Int](someValue.value)
        i <- ask[S, Long]
      } yield i.toInt + j

    // run effects
    val initial = init.value

    run(runReader(initial)(runDisjunction(readDisjunction))) must_==
      \/-(initial.toInt + someValue.value)

  }

  type DisjunctionString[A] = String \/ A

  def stacksafeRun = {
    type E = DisjunctionString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => DisjunctionEffect.right[E, String, String](i.toString))

    run(DisjunctionEffect.runDisjunction(action)) ==== \/-(list.map(_.toString))
  }

}

