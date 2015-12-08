package org.specs2
package control

import Eff._
import Effects._    
import Reader._
import Writer._
import Optional._  
import com.ambiata.disorder.PositiveIntSmall
import scalaz._, Scalaz._  

class EffSpec extends Specification with ScalaCheck { def is = s2"""

 run the reader monad with a pure operation $readerMonadPure
 run the reader monad with a bind operation $readerMonadBind
 run the writer monad twice                 $writerTwice
 run the optional monad                     $optionalMonad
 run the optional monad with nothing        $optionalWithNothingMonad
 run the optional monad with reader         $optionalReader
 
 run a reader/writer action $readerWriter

"""

  def readerMonadPure = prop { (initial: Int) =>
    run(runReader(ask[Reader[Int, ?] <:: EffectsNil, Int])(initial)) === initial
  }
 
  def readerMonadBind = prop { (initial: Int) =>
    type S = Reader[Int, ?] <:: EffectsNil
  
    val read: Eff[S, Int] = 
      for {
        i <- ask[S, Int]
        j <- ask[S, Int]
      } yield i + j
    
    run(runReader(read)(initial)) === initial * 2
  }
  
  def writerTwice = prop { _ : Int =>
    type S = Writer[String, ?] <:: EffectsNil
  
    val write: Eff[S, Unit] = 
      for {
        _ <- tell[S, String]("hello")
        _ <- tell[S, String]("world")
      } yield ()
    
    run(runWriter(write)) === ((), List("hello", "world"))
  }

  def readerWriter = prop { init: PositiveIntSmall =>
  
    // define a Reader / Writer stack
    type Stack[A, B] = Writer[A, ?] <:: Reader[B, ?] <:: EffectsNil
    type S1 = Stack[String, Int]
    
    implicit def ReaderStack[A, B]: Member[Reader[B, ?], Stack[A, B]] =
      Member.MemberNatIsMember[Reader[B, ?], Stack[A, B], S[Zero]]

    implicit def WriterStack[A, B]: Member[Writer[A, ?], Stack[A, B]] =
      Member.MemberNatIsMember[Writer[A, ?], Stack[A, B], Zero]
      
    // create actions
    val readWrite: Eff[S1, Int] = 
      for {
        i <- ask[S1, Int]
        _ <- tell[S1, String]("initial="+i)
        j <- ask[S1, Int]
        _ <- tell[S1, String]("result="+(i+j))
      } yield i + j
    
    // run effects
    val initial = init.value  
    run(runReader(runWriter(readWrite))(initial)) must_== 
      (initial * 2, List("initial="+initial, "result="+(initial*2)))
  }

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
