package org.specs2.control

import Eff._
import Effects._
import Member._
import scala.collection.mutable.ListBuffer
import scalaz.{-\/, \/-}

/**
 * Effect for computations possibly returning several values
 */
object ListEffect {

  /** create a list effect from a single value */
  def singleton[R, A](a: A)(implicit m: List <= R): Eff[R, A] =
    fromList(List(a))

  /** create a list effect from a list of values */
  def values[R, A](as: A*)(implicit m: List <= R): Eff[R, A] =
    fromList(as.toList)

  /** create a list effect from a list of values */
  def fromList[R, A](as: List[A])(implicit m: List <= R): Eff[R, A] =
    send[List, R, A](as)

  /** run an effect stack starting with a list effect */
  def runList[R <: Effects, A, B](effects: Eff[List |: R, A]): Eff[R, List[A]] = {
    def loop(eff: Eff[List |: R, A], unevaluated: List[Eff[List |: R, A]], result: ListBuffer[A]): Eff[R, List[A]] = {
      eff match {
        case Pure(a) =>
          unevaluated match {
            case head :: tail => loop(head, tail, result :+ a)
            case Nil          => EffMonad[R].point((result :+ a).toList)
          }

        case Impure(union, continuation) =>
          Union.decompose[List, R, union.X](union) match {
            case \/-(v) =>
              v match {
                case List() =>
                  unevaluated match {
                    case head :: tail => loop(head, tail, result)
                    case Nil          => EffMonad[R].point(result.toList)
                  }

                case head :: tail =>
                  loop(continuation(head), tail.asInstanceOf[List[A]].map(continuation.apply) ++ unevaluated, result)
              }

            case -\/(u1) =>
              Impure[R, u1.X, List[A]](u1, Arrs.singleton(x => loop(continuation(x), unevaluated, result)))
          }
      }
    }

    loop(effects, List(), new ListBuffer)
  }
}
