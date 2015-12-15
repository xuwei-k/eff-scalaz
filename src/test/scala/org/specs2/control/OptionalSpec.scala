package org.specs2
package control

import com.ambiata.disorder._
import Optional._
import Eff._
import Effects._
import Reader._  

import scalaz._, Scalaz._

class OptionalSpec extends Specification with ScalaCheck { def is = s2"""

 run the optional monad                     $optionalMonad
 run the optional monad with nothing        $optionalWithNothingMonad
 run the optional monad with reader         $optionalReader

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
    type Stack[A] = Optional <:: Reader[A, ?] <:: EffectsNil
    type S1 = Stack[Int]
    
    implicit def ReaderStack[A]: Member[Reader[A, ?], Stack[A]] =
      Member.MemberNatIsMember[Reader[A, ?], Stack[A], S[Zero]]

    implicit def OptionalStack[A]: Member[Optional, Stack[A]] =
      Member.MemberNatIsMember[Optional, Stack[A], Zero]
      
    // create actions
    val readOptional: Eff[S1, Int] = 
      for {
        j <- Optional.something[S1, Int](someValue.value)
        i <- ask[S1, Int]
      } yield i + j
    
    // run effects
    val initial = init.value  
    
    run(runReader(runOptional(readOptional))(initial)) must_== 
      Some(initial + someValue.value)
  }
}
