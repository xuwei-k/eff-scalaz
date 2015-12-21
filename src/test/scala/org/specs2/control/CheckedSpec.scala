package org.specs2
package control

import com.ambiata.disorder._
import Effects._
import Eff._
import Checked._
import ReaderEffect._
import Member._
import MemberNat._

import scalaz._, Scalaz._

class CheckedSpec extends Specification with ScalaCheck { def is = s2"""

 run the checked monad                     $checkedMonad
 run the checked monad with nothing        $checkedWithKoMonad
 run the checked monad with reader         $checkedReader

 run is stack safe with Checked      $stacksafeRun

"""

  def checkedMonad = {
    type S = CheckedString <:: EffectsNil

    implicit def CheckedStack: Member[CheckedString, S] =
      Member.MemberNatIsMember

    val checked: Eff[S, Int] =
      for {
        i <- Checked.ok[S, String, Int](1)
        j <- Checked.ok[S, String, Int](2)
      } yield i + j

    run(runChecked(checked)) === \/-(3)
  }

  def checkedWithKoMonad = {
    type S = CheckedString <:: EffectsNil

    implicit def CheckedStack: Member[CheckedString, S] =
      Member.MemberNatIsMember

    val checked: Eff[S, Int] =
      for {
        i <- Checked.ok[S, String, Int](1)
        j <- Checked.ko[S, String, Int]("error!")
      } yield i + j

    run(runChecked(checked)) === -\/("error!")
  }

  def checkedReader = prop { (init: PositiveLongSmall, someValue: PositiveIntSmall) =>

    // define a Reader / Checked stack
    type ReaderLong[A] = Reader[Long, A]
    type Stack = CheckedString <:: ReaderLong <:: EffectsNil

    implicit def ReaderStack: Member[ReaderLong, Stack] =
      Member.MemberNatIsMember

    implicit def CheckedStringStack: Member[CheckedString, Stack] =
      Member.MemberNatIsMember

    // create actions
    val readChecked: Eff[Stack, Int] =
      for {
        j <- Checked.ok(someValue.value)
        i <- ask[Stack, Long]
      } yield i.toInt + j

    // run effects
    val initial = init.value

    run(runReader(initial)(runChecked(readChecked))) must_==
      \/-(initial.toInt + someValue.value)
  }

  type CheckedString[A] = Checked[String, A]

  def stacksafeRun = {
    type E = CheckedString <:: EffectsNil
    implicit def CheckedStringMember: Member[CheckedString, E] =
      Member.MemberNatIsMember

    val list = (1 to 5000).toList
    val action = list.traverseU(i => Checked.ok(i.toString))

    run(Checked.runChecked(action)) ==== \/-(list.map(_.toString))
  }

}
