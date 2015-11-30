package org.specs2.control

import scalaz._, Scalaz._
import Eff._
import Effects._
import Member._
import Reader.{ask, runReader}
import Writer.{tell, runWriter}

object Example {

  def addGet[R](n: Int)(implicit member: Member[Reader[Int, ?], R]): Eff[R, Int] =
    ask[R, Int] >>= ((i: Int) => (i + n).point[Eff[R, ?]])

  def addN[R](n: Int)(implicit member: Member[Reader[Int, ?], R]): Eff[R, Int] =
    if (n == 0) addGet(0)
    else        addN(n - 1)(member) >>= (i => addGet(i)(member))


  /**
   * − rdwr :: (Member (Reader Int) r, Member (Writer String) r)
−− ⇒ Eff r Int
rdwr = do{ tell ”begin”; r ← addN 10; tell ”end”; return r }
   */
  def readWrite[R <: Effects](implicit member1: Member[Reader[Int, ?], R], member2: Member[Writer[String, ?], R]): Eff[R, Int] =
    for {
      _ <- tell("begin")
      r <- addN(10)
      _ <- tell("end")
    } yield r

  type Stack = Writer[String, ?] <:: Reader[Int, ?] <:: EffectsNil

  def test {

    implicit val m1: Member[Reader[Int, ?], Stack] = ???

    implicit val m2: Member[Writer[String, ?], Stack] =
      EffectMember[Writer[String, ?], Reader[Int, ?] <:: EffectsNil]

    runReader(runWriter(readWrite[Stack]))(10)
  }

}
