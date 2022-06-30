package cymbium.scratchpad

import cats.Monad
import com.sun.java.swing.plaf.windows.WindowsTableHeaderUI

import scala.annotation.tailrec

sealed trait Whynot[S] {
  type V
  //def run[A](a: Whynot.Action[A]): A
  def get(v: V): S
  def set(s: S)(v: V): Unit
}

object Whynot {
  def run[S,A](s: S)(a: Action[S,A]): A = {

    class WN extends Whynot[S] {
      class Ref { var x: S = s}
      override type V = Ref
      override def get(v: V) = v.x
      override def set(s: S)(v: V): Unit = v.x = s
    }
    val w = new WN
    a(w)(new w.Ref)
  }

  trait Action[S,A] {
    def apply(w: Whynot[S]): w.V => A
  }
  object Action {
    def get[S] = new Action[S,S] {
      override def apply(w: Whynot[S]): w.V => S = w.get _
    }
    def set[S](s: S) = new Action[S,Unit] {
      override def apply(w: Whynot[S]): w.V => Unit = w.set(s) _
    }


    implicit def actionIsAMonad[S]: Monad[({type X[A] = Action[S,A] })#X ] = new Monad[({type X[A] = Action[S,A] })#X ]
    {
      override def flatMap[A, B](fa: Action[S,A])(f: A => Action[S,B]): Action[S,B] = new Action[S,B] {
        def apply(w: Whynot[S]) = {v: w.V =>
          val aa = fa(w)(v)
          val fb = f(aa)
          fb(w)(v)
        }
      }

      override def tailRecM[A, B](a: A)(f: A => Action[S,Either[A, B]]): Action[S,B] = new Action[S,B] {
        override def apply(w: Whynot[S]): w.V => B = {v: w.V =>
          @tailrec def runA(a: A): B = {
            f(a)(w)(v) match {
              case Left(a2) => runA(a2)
              case Right(b) => b
            }
          }
          runA(a)
        }
      }

      override def pure[A](x: A): Action[S,A] = new Action[S,A] {
        def apply(w: Whynot[S]) = {_: w.V => x}
      }
    }
  }
}


object WhynotPlay {

  import cats.syntax.all._

  def main(as: Array[String]): Unit = {
    val inc = for (
      v <- Whynot.Action.get[Int];
      _ <- Whynot.Action.set(v + 1)
    ) yield v.toString

    println(Whynot.run(3)(inc))

    trait Test {
      val w: Whynot[Int]
      val v: w.V
    }
    val ext = new Whynot.Action[Int, Test] {
      override def apply(iw: Whynot[Int]): iw.V => Test = { iv =>
        iw.set(99)(iv)
        new Test {
          val w: iw.type = iw
          val v = iv
        }
      }
    }
    val extr = Whynot.run(3)(ext)
    println(extr)

    println(inc.apply(extr.w)(extr.v))

  }
}