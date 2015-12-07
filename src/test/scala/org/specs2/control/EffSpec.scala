package org.specs2
package control

import Eff._
import Effects._    
import Reader._

import scalaz._, Scalaz._  

class EffSpec extends Specification with ScalaCheck { def is = s2"""

 run the reader monad with a pure operation $readerMonadPure
 run the reader monad with a bind operation $readerMonadBind

"""

  def readerMonadPure = prop { (initial: Int) =>
    run(runReader(ask[Reader[Int, ?] <:: EffectsNil, Int])(initial)) === initial
  }
 
  def readerMonadBind= prop { (initial: Int) =>
    type S = Reader[Int, ?] <:: EffectsNil
  
    val read: Eff[S, Int] = 
      for {
        i <- ask[S, Int]
        j <- ask[S, Int]
      } yield i + j
    
    run(runReader(read)(initial)) === initial * 2
  }
  

}
