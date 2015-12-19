package org.specs2.control

import Eff._
import Effects._
import Member._

sealed trait Writer[O, X]

case class Put[O](private val run: () => O) extends Writer[O, Unit] {
  def value: O =
    run()
}

object Writer {

  def put[O](o: => O): Writer[O, Unit] =
    Put(() => o)

  def tell[R, O](o: => O)(implicit member: Member[Writer[O, ?], R]): Eff[R, Unit] =
    // the type annotation is necessary here to prevent a compiler error
    send[Writer[O, ?], R, Unit](put(o))


  /**
   * runWriter :: Eff (Writer o ’: r ) a → Eff r (a,[ o])
   * runWriter =
   * handle relay (\x → return (x,[]))
   * (\(Put o) k → k () >>= \(x,l ) → return (x, o: l ))
   */
  def runWriter[R <: Effects, O, A](w: Eff[Writer[O, ?] <:: R, A]): Eff[R, (A, List[O])] = {
    def loop(eff: Eff[Writer[O, ?] <:: R, A], l: List[O]): Eff[R, (A, List[O])] = {
      if (eff.isInstanceOf[Pure[Writer[O, ?] <:: R, A]])
         EffMonad[R].point((eff.asInstanceOf[Pure[Writer[O, ?] <:: R, A]].value, l))
      else {
        val i = eff.asInstanceOf[Impure[Writer[O, ?] <:: R, A]]
        val d = decompose[Writer[O, ?], R, A](i.union.asInstanceOf[Union[Writer[O, ?] <:: R, A]])
        if (d.toOption.isDefined)
          loop(i.continuation(()), l :+ d.toOption.get.asInstanceOf[Put[O]].value)
        else {
          val u = d.toEither.left.toOption.get
          Impure[R, (A, List[O])](u.asInstanceOf[Union[R, Any]], Arrs.singleton(x => loop(i.continuation(x), l)))
        }
      }
    }

    loop(w, List[O]())
  }

}
