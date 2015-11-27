package org.specs2.control

import scalaz._, Scalaz._
import Eff._
import Member._

trait Writer[O, X]

case class Put[O](o: O) extends Writer[O, Unit]

