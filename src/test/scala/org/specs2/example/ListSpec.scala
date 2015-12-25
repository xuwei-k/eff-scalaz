package org.specs2.example

import org.specs2._
import org.specs2.control.Eff._
import org.specs2.control.Effects._
import org.specs2.control.Member.<=
import org.specs2.control._

import scalaz.Scalaz._
import scalaz._

class ListSpec extends Specification { def is = s2"""

 list effect $listEffect

"""

  def listEffect = {
    object ListEffect {

      def singleton[R, A](a: A)(implicit m: List <= R): Eff[R, A] =
        fromList(List(a))

      def values[R, A](as: A*)(implicit m: List <= R): Eff[R, A] =
        fromList(as.toList)

      def fromList[R, A](as: List[A])(implicit m: List <= R): Eff[R, A] =
        send[List, R, A](as)

      def runList[R <: Effects, A, B](effects: Eff[List |: R, A]): Eff[R, List[A]] = {
        def loop(eff: Eff[List |: R, A], s: List[A]): Eff[R, List[A]] = {
          eff match {
            case Pure(a) =>
              EffMonad[R].point(s :+ a)

            case Impure(union, continuation) =>
              Union.decompose[List, R, union.X](union) match {
                case \/-(v) =>
                  v match {
                    case List() => EffMonad[R].point(s)
                    case head :: tail =>
                      // this implementation is *NOT* stack safe!
                      for {
                        hs <- loop(continuation(head), s)
                        ts <- loop(impure(UnionNow(tail), continuation), s)
                      } yield hs ++ ts
                  }

                case -\/(u1) =>
                  Impure[R, u1.X, List[A]](u1, Arrs.singleton(x => loop(continuation(x), s)))
              }
          }
        }

        loop(effects, List[A]())
      }
    }

    import ListEffect._

    type L = List |: NoEffect
    implicit def ListMember: List <= L =
      Member.MemberNatIsMember

    val action: Eff[L, Int] = for {
      a <- singleton(1)
      b <- fromList(List.fill(5)(a))
    } yield b + 1

    (action |> runList |> run) ==== List(2, 2, 2, 2, 2)
  }
}
