package org.specs2.control

import org.specs2.Specification
import Eff._
import Effects._
import scalaz._, Scalaz._

class DisjunctionErrorEffectSpec extends Specification { def is = s2"""

  Writer can be used with Eval to get logs even if there is an exception $logException

"""

  def logException = {
    import EvalEffect._
    import DisjunctionErrorEffect._
    import WriterEffect._

    type WriterString[A] = Writer[String, A]

    type E = DisjunctionError |: WriterString |: Eval |: NoEffect

    implicit def WriterStringMember: Member[WriterString, E] =
      Member.MemberNatIsMember

    implicit def DisjunctionErrorMember: Member[DisjunctionError, E] =
      Member.MemberNatIsMember

    val action: Eff[E, Int] = for {
      _ <- tell[E, String]("start")
      a <- DisjunctionErrorEffect.ok[E, Int] { throw new Exception("boom"); 1 }
      _ <- tell[E, String]("end")
    } yield a

    val result =
      run(runEval(runWriter(runDisjunctionError(action))))

    (result._2 ==== List("start")) and
    (result._1.toOptionErrorSimpleMessage ==== Option("Error[java.lang.Exception] boom"))
  }

}
