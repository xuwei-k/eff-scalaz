package org.specs2.eff

import org.specs2.Specification
import Effects._
import org.specs2.eff.EvalEffect.Eval
import org.specs2.matcher.DisjunctionMatchers

import scalaz.{Scalaz, Need, Writer, Reader}

class MemberSpec extends Specification with DisjunctionMatchers { def is = s2"""

 inject / project must work at the value level
   for reader $reader
   for writer $writer
   for eval   $eval

"""
  type WriterString[A] = Writer[String, A]
  type ReaderInt[A] = Reader[Int, A]

  type S = WriterString |: ReaderInt |: Eval |: NoEffect

  implicit def writerMember =
    Member.aux[WriterString, S, ReaderInt |: Eval |: NoEffect]

  implicit def readerMember =
    Member.aux[ReaderInt, S, WriterString |: Eval |: NoEffect]

  implicit def evalMember =
    Member.aux[Eval, S, WriterString |: ReaderInt |: NoEffect]

  def reader = {
    val read1 = Reader((i: Int) => "hey")
    readerMember.project(readerMember.inject(read1)) must be_\/-(read1)
  }

  def writer = {
    val write1 = Writer("hey", ())
    writerMember.project(writerMember.inject(write1)) must be_\/-(write1)
  }

  def eval = {
    val eval1 = Need("hey")
    evalMember.project(evalMember.inject(eval1)) must be_\/-(eval1)
  }

}
