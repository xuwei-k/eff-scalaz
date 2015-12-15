package org.specs2
package control

import com.ambiata.disorder._
import Effects._
import Eff._
import Checked._
import Reader._
import Member._
import MemberNat._    

import scalaz._, Scalaz._

class CheckedSpec extends Specification with ScalaCheck { def is = s2"""

 run the checked monad                     $checkedMonad
 run the checked monad with nothing        $checkedWithKoMonad
 run the checked monad with reader         $checkedReader

"""

  def checkedMonad = {
    type S = Checked[String, ?] <:: EffectsNil
  
    val checked: Eff[S, Int] = 
      for {
        i <- Checked.ok[S, String, Int](1)
        j <- Checked.ok[S, String, Int](2)
      } yield i + j
    
    run(runChecked(checked)) === Right(3)
  }

  def checkedWithKoMonad = {
    type S = Checked[String, ?] <:: EffectsNil
  
    val checked: Eff[S, Int] = 
      for {
        i <- Checked.ok[S, String, Int](1)
        j <- Checked.ko[S, String, Int]("error!")
      } yield i + j
    
    run(runChecked(checked)) === Left("error!")
  }
  
  def checkedReader = prop { (init: PositiveLongSmall, someValue: PositiveIntSmall) =>
  
    // define a Reader / Checked stack
    type Stack[A] = Checked[String, ?] <:: Reader[A, ?] <:: EffectsNil
    type S1 = Stack[Long]
    
    implicit def ReaderStack[A]: Member[Reader[A, ?], Stack[A]] =
      Member.MemberNatIsMember[Reader[A, ?], Stack[A], S[Zero]](
        Reader.ReaderMemberNatS[Checked[String, ?], Reader[A, ?] <:: EffectsNil, Zero, A],
        P[S[Zero]]  
      )
    
    implicit def CheckedStack[A]: Member[Checked[String, ?], Stack[A]] =
      Member.MemberNatIsMember[Checked[String, ?], Stack[A], Zero]
      
    // create actions
    val readChecked: Eff[S1, Int] = 
      for {
        j <- Checked.ok[S1, String, Int](someValue.value)
        i <- ask[S1, Long]
      } yield i.toInt + j
    
    // run effects
    val initial = init.value  
    
    run(runReader(runChecked(readChecked))(initial)) must_== 
      Right(initial.toInt + someValue.value)
  }
}
