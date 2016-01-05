package org.specs2.example

import org.specs2.Specification
import org.specs2.control.eff._
import Eff._
import Effects._
import EvalEffect._
import OptionEffect._
import scala.collection.mutable.ListBuffer
import scalaz._, Scalaz._

class RecursiveSpec extends Specification { def is = s2"""

 Run a recursively defined effect $recursive

"""

  def recursive = {
    val Size = 10000
    type S = Option |: Eval |: NoEffect

    val elements = (1 to Size).iterator
    val result = new ListBuffer[Int]

    def read: Eff[S, Int] =
      if (elements.hasNext) OptionEffect.some(elements.next)
      else OptionEffect.none

    def write(i: Int): Eff[S, Unit] =
      delay(result.append(i))

    def readWrite: Eff[S, Unit] = for {
      l <- read
      _ <- write(l)
      _ <- readWrite
    } yield ()

    readWrite |> runOption |> runEval |> run

    result.toList === (1 to Size).toList

  }

}
