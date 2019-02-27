package cymbium.q2

import algebra.ring._
import algebra.Eq
import cymbium.q2
import cymbium.q2.Q2.Q2Double

import scala.{specialized => sp}


object Q2Instances {

  def main(as: Array[String]): Unit = {
    import spire.std.double._

    implicit val ringer = new q2.Q2Instances.Q2CommutativeRing.Std[Q2Double,Double]

    import spire.syntax.ring._

    val x = Q2(3.0,9.0)

    val y = Q2(10.0,22.0)

    println(x + y)


  }

  //TODO:  Specializing on A in these traits doesn't cause these functions to call
  // the specialized versions of the methods in Q2Functions.  However, defining
  // non-generic type classes does allow the specialized methods to be called.
  // So, not going to worry about specializaiton for these classes for right now...
  // Maybe I'll code-generate primitive versions later.
  // Or actually..macro expansion could be a good alternative.  Since we only care
  // about implementation, black-box macros should suffice

  trait Q2Eq[T, A] extends ExtendToQ2[T,A, Eq] with Eq[T] {
    override def eqv(x: T, y: T): Boolean = Q2Functions.eqv(x,y)
    override def neqv(x: T, y: T): Boolean = Q2Functions.neqv(x,y)
  }

  object Q2Eq {
    class Std[T,A](
      implicit val q2Like: Q2Like[T,A],
      implicit val baseAlgebraic: Eq[A]
    ) extends Q2Eq[T,A]
  }

  
  
  trait Q2AdditiveSemigroup[T, A] extends ExtendToQ2[T,A,AdditiveSemigroup] with AdditiveSemigroup[T] {
    override def plus(x: T, y: T): T = Q2Functions.plus(x,y)
    override def sumN(a: T, n: Int): T = Q2Functions.sumN(a,n)
  }
  object Q2AdditiveSemigroup {
    class Std[T,A](
      implicit val q2Like: Q2Like[T,A],
      implicit val baseAlgebraic: AdditiveSemigroup[A]
    ) extends Q2AdditiveSemigroup[T,A]
  }


  trait Q2AdditiveMonoid[T, A] extends ExtendToQ2[T,A,AdditiveMonoid]
    with Q2AdditiveSemigroup[T,A] with AdditiveMonoid[T] {
    override def zero: T = Q2Functions.zero
    override def sumN(a: T, n: Int): T = Q2Functions.sumN(a,n)
  }
  object Q2AdditiveMonoid {
    class Std[T,A](
                    implicit val q2Like: Q2Like[T,A],
                    implicit val baseAlgebraic: AdditiveMonoid[A]
                  ) extends Q2AdditiveMonoid[T,A]
  }

  trait Q2AdditiveGroup[T, A] extends ExtendToQ2[T,A,AdditiveGroup]
    with Q2AdditiveMonoid[T,A]
    with AdditiveGroup[T] {
    override def negate(x: T) = Q2Functions.negate(x)
    override def minus(x: T, y: T) = Q2Functions.minus(x,y)
  }
  object Q2AdditiveGroup {
    class Std[T,A](
                    implicit val q2Like: Q2Like[T,A],
                    implicit val baseAlgebraic: AdditiveGroup[A])
      extends Q2AdditiveGroup[T,A]
  }


  trait Q2AdditiveCommutativeSemigroup[T, A] extends ExtendToQ2[T,A,AdditiveCommutativeSemigroup]
    with Q2AdditiveSemigroup[T, A]
    with AdditiveCommutativeSemigroup[T]

  object Q2AdditiveCommutativeSemigroup {
    class Std[T,A](
      implicit val q2Like: Q2Like[T,A],
      implicit val baseAlgebraic: AdditiveCommutativeSemigroup[A]
    ) extends Q2AdditiveCommutativeSemigroup[T,A]
  }

  trait Q2AdditiveCommutativeMonoid[T, A] extends ExtendToQ2[T,A,AdditiveCommutativeMonoid]
    with Q2AdditiveMonoid[T,A]
    with Q2AdditiveCommutativeSemigroup[T,A]
    with AdditiveCommutativeMonoid[T]

  object Q2AdditiveCommutativeMonoid {
    class Std[T,A](
      implicit val q2Like: Q2Like[T,A],
      implicit val baseAlgebraic: AdditiveCommutativeMonoid[A]
    ) extends Q2AdditiveCommutativeMonoid[T,A]
  }

  
  trait Q2AdditiveCommutativeGroup[T, A] extends ExtendToQ2[T,A,AdditiveCommutativeGroup]
    with Q2AdditiveGroup[T,A]
    with Q2AdditiveCommutativeMonoid[T,A] with  AdditiveCommutativeGroup[T]
  object Q2AdditiveCommutativeGroup {
    class Std[T,A](implicit val q2Like: Q2Like[T,A],
                   val baseAlgebraic: AdditiveCommutativeGroup[A]) extends Q2AdditiveCommutativeGroup[T,A]
  }

  
  trait Q2Semiring[T,A] extends ExtendToQ2[T,A,Semiring]
      with Q2AdditiveCommutativeMonoid[T,A] with Semiring[T] {
    override def times(x: T, y: T)= Q2Functions.times(x,y)
  }
  object Q2Semiring {
    class Std[T,A](implicit val q2Like: Q2Like[T,A],
                   val baseAlgebraic: Semiring[A]) extends Q2Semiring[T,A]
  }

  trait Q2Rig[T,A] extends ExtendToQ2[T,A,Rig]
    with Q2Semiring[T,A] with Rig[T] {
    override def one: T = Q2Functions.one
  }
  object Q2Rig {
    class Std[T,A](implicit val q2Like: Q2Like[T,A],
                   val baseAlgebraic: Rig[A]) extends Q2Rig[T,A]
  }

  trait Q2Rng[T,A] extends ExtendToQ2[T,A,Rng]
    with Q2Semiring[T,A]
    with Q2AdditiveCommutativeGroup[T,A]
    with Rng[T]
  object Q2Rng {
    class Std[T,A](implicit val q2Like: Q2Like[T,A],
                   val baseAlgebraic: Rng[A]) extends Q2Rng[T,A]
  }

  trait Q2Ring[T,A] extends ExtendToQ2[T,A,Ring]
    with Q2Rng[T,A] with Q2Rig[T,A]
    with Ring[T] {
    override def fromBigInt(n: BigInt): T = Q2Functions.fromBigInt(n)
    override def fromInt(n: Int): T = Q2Functions.fromInt(n)
  }
  object Q2Ring {
    class Std[T,A](implicit val q2Like: Q2Like[T,A],
                   val baseAlgebraic: Ring[A]) extends Q2Ring[T,A]
  }



  trait Q2CommutativeSemiring[T,A] extends ExtendToQ2[T,A,CommutativeSemiring]
    with Q2Semiring[T,A] with CommutativeSemiring[T]
  object Q2CommutativeSemiring {
    class Std[T,A](implicit val q2Like: Q2Like[T,A],
                   val baseAlgebraic: CommutativeSemiring[A]) extends Q2CommutativeSemiring[T,A]
  }






  trait Q2CommutativeRig[T,A] extends ExtendToQ2[T,A,CommutativeRig]
    with Q2CommutativeSemiring[T,A]
    with Q2Rig[T,A] with CommutativeRig[T]
  object Q2CommutativeRig {
    class Std[T,A](implicit val q2Like: Q2Like[T,A],
                   val baseAlgebraic: CommutativeRig[A]) extends Q2CommutativeRig[T,A]
  }



  trait Q2CommutativeRng[T,A] extends ExtendToQ2[T,A,CommutativeRng]
    with Q2CommutativeSemiring[T,A]
    with Q2Rng[T,A] with CommutativeRng[T]
  object Q2CommutativeRng {
    class Std[T,A](implicit val q2Like: Q2Like[T,A],
                   val baseAlgebraic: CommutativeRng[A]) extends Q2CommutativeRng[T,A]
  }


  trait Q2CommutativeRing[T,A] extends ExtendToQ2[T,A,CommutativeRing]
    with Q2Ring[T,A] with Q2CommutativeRig[T,A] with Q2CommutativeRng[T,A]
    with CommutativeRing[T]
  object Q2CommutativeRing{
    class Std[T,A](implicit val q2Like: Q2Like[T,A],
                   val baseAlgebraic: CommutativeRing[A]) extends Q2CommutativeRing[T,A]
  }


}
