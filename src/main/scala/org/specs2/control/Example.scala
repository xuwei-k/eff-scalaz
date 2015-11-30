package org.specs2.control

import scalaz.{Coproduct=>_,Inject=>_,:+: => _,_}, Scalaz._
import Eff._
import Effects._
import Members._
import shapeless._, ops.coproduct._
import Reader.{ask, runReader}
import Writer.{tell, runWriter}

object Example {

  def addGet[R <: Coproduct](n: Int)(implicit member: Members[Reader[Int, ?], R], inject: Inject[R, Reader[Int, Int]]): Eff[R, Int] =
    ask[R, Int] >>= ((i: Int) => (i + n).point[Eff[R, ?]])

  def addN[R <: Coproduct](n: Int)(implicit member: Members[Reader[Int, ?], R], inject: Inject[R, Reader[Int, Int]]): Eff[R, Int] =
    if (n == 0) addGet(0)
    else        addN(n - 1)(member, inject) >>= (i => addGet(i)(member, inject))


  /**
   * − rdwr :: (Member (Reader Int) r, Member (Writer String) r)
−− ⇒ Eff r Int
rdwr = do{ tell ”begin”; r ← addN 10; tell ”end”; return r }
   */
  def readWrite[R <: Coproduct](implicit member1: Members[Reader[Int, ?], R], inject1: Inject[R, Reader[Int, Int]],
                                         member2: Members[Writer[String, ?], R], inject2: Inject[R, Writer[String, Unit]]): Eff[R, Int] =
    for {
      _ <- tell("begin")
      r <- addN(10)
      _ <- tell("end")
    } yield r

  type Stack = Writer[String, Int] :+: Reader[Int, Int] :+: CNil

  def test {

    implicit val m1: Members[Reader[Int, ?], Stack] = ???

    implicit val m2: Members[Writer[String, ?], Stack] =
      ??? // EffectMember[Writer[String, ?], Reader[Int, ?] <:: EffectsNil]

    implicit val inject1: Inject[Stack, Reader[Int, Int]] = ???
    implicit val inject2: Inject[Stack, Writer[String, Unit]] = ???

    runReader(
    runWriter(readWrite[Stack])
    )(10)
  }

}
