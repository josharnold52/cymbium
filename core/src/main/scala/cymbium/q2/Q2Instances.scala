package cymbium.q2

import algebra.ring._
import algebra.Eq

import scala.{specialized => sp}


object Q2Instances {

  def main(as: Array[String]): Unit = {
    import spire.std.double._

    val x = Q2(3.0,9.0)

    val y = Q2(10.0,22.0)

    println(Q2Functions.plus(x,y))


    val q1 = Q2(9L,10L)
    println(Try1.eqv(q1,q1))
  }

  //TODO:  Specializing on A in these traits doesn't cause these functions to call
  // the specialized versions of the methods in Q2Functions.  However, defining
  // non-generic type classes does allow the specialized methods to be called.
  // So, not going to worry about specializaiton for these classes for right now...
  // Maybe I'll code-generate primitive versions later.
  // Or actually..macro expansion could be a good alternative.  Since we only care
  // about implementation, black-box macros should suffice

  trait Q2Eq[T, A] extends Eq[T] {
    implicit val q2: Q2Like[T, A]
    implicit val base: Eq[A]

    def eqv(x: T, y: T): Boolean = Q2Functions.eqv(x,y)
    override def neqv(x: T, y: T): Boolean = Q2Functions.neqv(x,y)
  }

  trait Q2AdditiveSemigroup[T, A] extends AdditiveSemigroup[T] {
    implicit val q2: Q2Like[T, A]
    implicit val base: AdditiveSemigroup[A]

    def plus(x: T, y: T): T = Q2Functions.plus(x,y)

    override def sumN(a: T, n: Int): T = Q2Functions.sumN(a,n)
  }

  trait Q2AdditiveCommutativeSemigroup[T, A] extends Q2AdditiveSemigroup[T, A] with AdditiveCommutativeSemigroup[T] {
    implicit val base: AdditiveCommutativeSemigroup[A]
  }

  trait Q2AdditiveMonoid[T, A] extends Q2AdditiveSemigroup[T,A] with AdditiveMonoid[T] {

    implicit val base: AdditiveMonoid[A]

    def zero: T = Q2Functions.zero

    override def sumN(a: T, n: Int): T = Q2Functions.sumN(a,n)
  }

  trait Q2AdditiveCommutativeMonoid[T, A] extends Q2AdditiveMonoid[T,A]
    with Q2AdditiveCommutativeSemigroup[T,A] with  AdditiveCommutativeMonoid[T] {
    implicit val base: AdditiveCommutativeMonoid[A]
  }


  trait Q2AdditiveGroup[T, A] extends Q2AdditiveMonoid[T,A] with AdditiveGroup[T] {

    implicit val base: AdditiveGroup[A]

    def negate(x: T) = Q2Functions.negate(x)

  }

  trait Q2AdditiveCommutativeGroup[T, A] extends Q2AdditiveGroup[T,A]
    with Q2AdditiveCommutativeMonoid[T,A] with  AdditiveCommutativeGroup[T] {
    implicit val base: AdditiveCommutativeGroup[A]
  }

  trait Q2Semiring[T,A] extends Q2AdditiveCommutativeMonoid[T,A] with Semiring[T] {
    implicit val base: Semiring[A]

    def times(x: T, y: T)= Q2Functions.times(x,y)
  }

  trait Q2CommutativeSemiring[T,A] extends Q2Semiring[T,A] with CommutativeSemiring[T] {
    implicit val base: CommutativeSemiring[A]

    }

  object Try1 extends Q2Eq[Q2.Q2Long, Long] {
    val q2 = Q2Like.Q2LongIsQ2Like
    val base = spire.std.long.LongAlgebra
  }

}
