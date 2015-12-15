package org.specs2
package controls

import scalaz.{Writer => _, Reader => _,_}, Scalaz._ 
import control._  

import Eff._
import Effects._
import Writer._
import Reader._
import Eval._  
  
object Action {

  trait ConsoleTag
  trait WarningsTag

  type Console[A] = Writer[String, A] @@ ConsoleTag
  type Warnings[A] = Writer[String, A] @@ WarningsTag
  type CheckedString[A] = Checked[String, A]

  type ActionStack = Console <:: Warnings <:: CheckedString <:: Eval <:: EffectsNil

  implicit def EvalEffect: Member[Eval, ActionStack] = new Member[Eval, ActionStack] {
    def inject[V](tv: Eval[V]): Union[ActionStack, V] = 
      UnionNext(UnionNext(UnionNext(UnionNow(tv))))  
    def project[V](u: Union[ActionStack, V]): Option[Eval[V]] = 
      u match {
        case UnionNow(x) => None
        case UnionNext(UnionNext(UnionNext(UnionNow(e)))) => Some(e)
      }
  } 
     
  implicit def CheckedStringEffect: Member[CheckedString, ActionStack] = new Member[CheckedString, ActionStack] {
    def inject[V](tv: CheckedString[V]): Union[ActionStack, V] = 
      UnionNext(UnionNext(UnionNow(tv)))  
    def project[V](u: Union[ActionStack, V]): Option[CheckedString[V]] = 
      u match {
        case UnionNow(x) => None
        case UnionNext(UnionNext(UnionNow(e))) => Some(e)
      }
  } 
  
  implicit def WarningsEffect: Member[Warnings, ActionStack] = new Member[Warnings, ActionStack] {
    def inject[V](tv: Warnings[V]): Union[ActionStack, V] = 
      UnionNext(UnionNow(tv))  
    def project[V](u: Union[ActionStack, V]): Option[Warnings[V]] = 
      u match {
        case UnionNow(x) => None
        case UnionNext(UnionNow(e)) => Some(e)
      }
  } 

  implicit def ConsoleEffect: Member[Console, ActionStack] = new Member[Console, ActionStack] {
    def inject[V](tv: Console[V]): Union[ActionStack, V] = 
      Union.now(tv)  
    def project[V](u: Union[ActionStack, V]): Option[Console[V]] = 
      u match {
        case UnionNow(x) => Some(x)
        case _ => None
      }
  } 
  
  def log[R <: Effects](message: String, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] = 
    if (doIt) Writer.tell(message)(Member.untagMember[Writer[String, ?], R, ConsoleTag](m))
    else      EffMonad.point(())
    
  def logThrowable[R <: Effects](t: Throwable, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) logThrowable(t) 
    else      EffMonad.point(())

  def logThrowable[R <: Effects](t: Throwable)(implicit m: Member[Console, R]): Eff[R, Unit] =
    log(t.getMessage, doIt = true)(m) >>
    log(t.getStackTrace.mkString("\n"), doIt = true) >>
      (if (t.getCause != null) logThrowable(t.getCause)
       else                    EffMonad.point(()))
       
       
  /** warn the user about something that is probably wrong on his side, this is not a specs2 bug */
  def warn[R <: Effects](message: String)(implicit m: Member[Warnings, R]): Eff[R, Unit] =
    Writer.tell(message)(Member.untagMember[Writer[String, ?], R, WarningsTag](m))

  /**
   * warn the user about something that is probably wrong on his side,
   * this is not a specs2 bug, then fail to stop all further computations
   */
  def warnAndFail[R <: Effects, A](message: String, failureMessage: String)(implicit m1: Member[Warnings, R], m2: Member[CheckedString, R]): Eff[R, A] =
    warn(message)(m1) >> 
    Checked.ko(failureMessage)


  /**
   * This interpreter prints messages to the console
   */
  def runConsole[R <: Effects, A](w: Eff[Console <:: R, A]): Eff[R, A] = 
    runConsoleToPrinter(m => println(m))(w)

  /**
   * This interpreter prints messages to a printing function
   */
  def runConsoleToPrinter[R <: Effects, A](printer: String => Unit): Eff[Console <:: R, A] => Eff[R, A] = {
    val putRest = new EffCont[Console, R, A] {
      def apply[X](w: Console[X])(continuation: X => Eff[R, A]): Eff[R, A] = Tag.unwrap(w) match {
        case Put(m) => printer(m()); continuation(())
      }
    }

    relay1((a: A) => a)(putRest)
  }

  /**
   * This interpreter cumulates warnings
   */
  def runWarnings[R <: Effects, A](w: Eff[Warnings <:: R, A]): Eff[R, (A, Vector[String])] = {
    val putRest = new EffCont[Warnings, R, (A, Vector[String])] {
      def apply[X](w: Warnings[X])(continuation: X => Eff[R, (A, Vector[String])]): Eff[R, (A, Vector[String])] = Tag.unwrap(w) match {
        case Put(m) => continuation(()) >>= (al => EffMonad[R].point((al._1, al._2 :+ m())))
      }
    }

    relay1((a: A) => (a, Vector[String]()))(putRest)(w)
  }
     
}
