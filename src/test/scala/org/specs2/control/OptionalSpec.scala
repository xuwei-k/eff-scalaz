package org.specs2
package control

import com.ambiata.disorder._
import Optional._
import Eff._
import Effects._
import Reader._

import scalaz.{Optional=>_, Reader=>_,_}, Scalaz._

class OptionalSpec extends Specification with ScalaCheck { def is = s2"""

 run the optional monad                     $optionalMonad
 run the optional monad with nothing        $optionalWithNothingMonad
 run the optional monad with reader         $optionalReader

 The Eff monad is stack safe with Optional  $stacksafeOptional

"""

  def optionalMonad = {
    type S = Optional <:: EffectsNil

    val optional: Eff[S, String] =
      for {
        s1 <- Optional.something[S, String]("hello")
        s2 <- Optional.something[S, String]("world")
      } yield s1 + " " + s2

    run(runOptional(optional)) === Some("hello world")
  }

  def optionalWithNothingMonad = {
    type S = Optional <:: EffectsNil

    val optional: Eff[S, String] =
      for {
        s1 <- Optional.something[S, String]("hello")
        s2 <- Optional.nothing[S, String]
      } yield s1 + " " + s2

    run(runOptional(optional)) === None
  }

  def optionalReader = prop { (init: PositiveIntSmall, someValue: PositiveIntSmall) =>

    // define a Reader / Optional stack
    type R[A] = Reader[Int, A]
    type S = Optional <:: R <:: EffectsNil
    import Member._

    implicit def ReaderStackMember: Member[R, S] =
      Member.MemberNatIsMember

    // create actions
    val readOptional: Eff[S, Int] =
      for {
        j <- Optional.something[S, Int](someValue.value)
        i <- ask[S, Int]
      } yield i + j

    // run effects
    val initial = init.value

    run(runReader(initial)(runOptional(readOptional))) must_==
      Some(initial + someValue.value)
  }

  def stacksafeOptional = {
    type E = Optional <:: EffectsNil
    implicit def OptionalMember: Member[Optional, E] =
      Member.MemberNatIsMember

    val list = (1 to 5000).toList
    val action = list.traverseU(i => Optional.something(i))

    run(Optional.runOptional(action)) ==== Some(list)
  }

}
