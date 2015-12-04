package org.specs2.control

import scalaz._
import scalaz.syntax.monad._        
import Effects.{<::}
import Member.MemberNatIsMember
import MemberNat.{ZeroMemberNat, SuccessorMemberNat}  
import Writer.{tell, runWriter}
import Reader.{ask, runReader}

object Example {

  def addGet[R](n: Int)(implicit member: Member[Reader[Int, ?], R]): Eff[R, Int] =
    ask[R, Int] >>= ((i: Int) => (i + n).point[Eff[R, ?]])

  def addN[R](n: Int)(implicit member: Member[Reader[Int, ?], R]): Eff[R, Int] =
    if (n == 0) addGet(0)
    else        addN(n - 1)(member) >>= (i => addGet(i)(member))


  /**
   *  rdwr :: (Member (Reader Int) r, Member (Writer String) r) ⇒ Eff r Int
   *  rdwr = do{ tell ”begin”; r ← addN 10; tell ”end”; return r }
   */
  def readWrite[R <: Effects](implicit member1: Member[Reader[Int, ?], R], member2: Member[Writer[String, ?], R]): Eff[R, Int] =
    for {
      _ <- tell("begin")
      r <- addN(10)
      _ <- tell("end")
    } yield r

  type Stack[A, B] = Writer[A, ?] <:: Reader[B, ?] <:: EffectsNil

  def test {

    implicit def ReaderStack[A, B]: Member[Reader[B, ?], Stack[A, B]] =
      MemberNatIsMember[Reader[B, ?], Stack[A, B], S[Zero]]

    implicit def WriterStack[A, B]: Member[Writer[A, ?], Stack[A, B]] =
      MemberNatIsMember[Writer[A, ?], Stack[A, B], Zero]

    runReader(runWriter(readWrite[Stack[String, Int]]))(10)
  }

}
