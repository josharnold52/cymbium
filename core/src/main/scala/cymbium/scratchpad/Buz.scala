package cymbium.scratchpad

import cats.{Contravariant, Functor, Monad}
import scala.language.higherKinds

object Buz {

  def abc(): Unit = {
    import cats._
    import cats.instances.all._
    val x= implicitly[Semigroup[Int]]

    val i = implicitly[Invariant[Semigroup]]
    //val
  }

  trait ScCh[S[_],D[_]] {

    import cats.syntax.all._

    implicit def sContra: Contravariant[S]
    implicit def dFunctor: Monad[D]



    type M[A] = S[A] => D[A]


    def lifter[A](sa: S[A]): S[M[A]]


    def mmap[A,B](m: M[A])(f: A => B): M[B] = { sa => m(sa.contramap(f)).map(f) }

    //def cMonad: Monad[C]

    def mflatten[A](m: M[M[A]]): M[A] = { sa =>
      val x = m(lifter(sa))
      val y = x.flatMap{ _.apply(sa) }
      y


    }

  }

}
