package cymbium.q2

import algebra.Eq
import algebra.ring._

import scala.{specialized => sp}

trait Q2Like[T, @sp(Int,Long,Float,Double) A] { self =>
  def a(t: T): A
  def b(t: T): A
  def of(a: A, b: A): T
}

object Q2Like {

  implicit object Q2DoubleIsQ2Like extends Q2Like[Q2.Q2Double, Double] {
    type A = Double
    def a(q: Q2.Q2Double): Double = q.a
    def b(q: Q2.Q2Double): Double = q.b
    def of(a: Double, b: Double): Q2.Q2Double = Q2.Q2Double(a,b)
  }
  implicit object Q2FloatIsQ2Like extends Q2Like[Q2.Q2Float, Float] {
    type A = Float
    def a(q: Q2.Q2Float): Float = q.a
    def b(q: Q2.Q2Float): Float = q.b
    def of(a: Float, b: Float): Q2.Q2Float = Q2.Q2Float(a,b)
  }
  implicit object Q2IntIsQ2Like extends Q2Like[Q2.Q2Int, Int] {
    type A = Int
    def a(q: Q2.Q2Int): Int = q.a
    def b(q: Q2.Q2Int): Int = q.b
    def of(a: Int, b: Int): Q2.Q2Int = Q2.Q2Int(a,b)
  }
  implicit object Q2LongIsQ2Like extends Q2Like[Q2.Q2Long, Long] {
    type A = Long
    def a(q: Q2.Q2Long): Long = q.a
    def b(q: Q2.Q2Long): Long = q.b
    def of(a: Long, b: Long): Q2.Q2Long = Q2.Q2Long(a,b)
  }

  class Q2RefIsQ2Like[A2 <: AnyRef] extends Q2Like[Q2.Q2Ref[A2], A2] {
    type A = A2
    def a(q: Q2.Q2Ref[A2]): A2 = q.a
    def b(q: Q2.Q2Ref[A2]): A2 = q.b
    def of(a: A2, b: A2): Q2.Q2Ref[A2] = Q2.Q2Ref(a,b)

  }
  object q2RefAnyIsQ2Like extends Q2RefIsQ2Like[AnyRef]

  implicit def q2RefAIsQ2Like[A <: AnyRef]: Q2RefIsQ2Like[A] = q2RefAnyIsQ2Like.asInstanceOf[Q2RefIsQ2Like[A]]


  class Q2GenericIsQ2Like[A2] extends Q2Like[Q2.Q2Generic[A2], A2] {
    type A = A2
    def a(q: Q2.Q2Generic[A2]): A2 = q.a
    def b(q: Q2.Q2Generic[A2]): A2 = q.b
    def of(a: A2, b: A2): Q2.Q2Generic[A2] = Q2.Q2Generic(a,b)

  }

}

trait Q2[@sp(Int,Long,Float,Double) A] {
  def a: A
  def b: A
}



object Q2 {

  case class Q2Double(a: Double, b: Double) extends Q2[Double] {
    def build(a: Double, b: Double) = Q2Double(a,b)
  }
  case class Q2Long(a: Long, b: Long) extends Q2[Long]{
    def build(a: Long, b: Long) = Q2Long(a,b)
  }
  case class Q2Int(a: Int, b: Int) extends Q2[Int]{
    def build(a: Int, b: Int) = Q2Int(a,b)
  }
  case class Q2Float(a: Float, b: Float) extends Q2[Float]{
    def build(a: Float, b: Float) = Q2Float(a,b)
  }
  case class Q2Ref[A <: AnyRef](a: A, b: A) extends Q2[A]{
    def build(a: A, b: A) = Q2Ref(a,b)
  }
  case class Q2Generic[A](a: A, b: A) extends Q2[A]{
    def build(a: A, b: A) = Q2Generic(a,b)
  }

  def apply(a: Int, b: Int) = Q2Int(a,b)
  def apply(a: Long, b: Long) = Q2Long(a,b)
  def apply(a: Double, b: Double) = Q2Double(a,b)
  def apply(a: Float, b: Float) = Q2Float(a,b)
  def apply[A <: AnyRef](a: A, b: A) = Q2Ref(a,b)




}

object Q2Functions {
  def zero[T, A : AdditiveMonoid](implicit q2: Q2Like[T,A]): T = {
    val am = implicitly[AdditiveMonoid[A]]
    q2.of(am.zero,am.zero)
  }

  def plus[T, @sp(Int,Long,Float,Double) A : AdditiveSemigroup](x: T, y: T)(implicit q2: Q2Like[T,A]): T = {
    import spire.syntax.additiveSemigroup._

    val az = q2.a(x) + q2.a(y)
    val bz = q2.b(x) + q2.b(y)

    q2.of(az,bz)
  }

  def minus[T, @sp(Int,Long,Float,Double) A : AdditiveGroup](x: T, y:T)(implicit q2: Q2Like[T,A]): T = {
    import spire.syntax.additiveGroup._
    q2.of(q2.a(x) - q2.a(y), q2.b(x) - q2.b(y))
  }

  def negate[T, @sp(Int,Long,Float,Double) A : AdditiveGroup](x: T)(implicit q2: Q2Like[T,A]): T = {
    import spire.syntax.additiveGroup._
    q2.of(-q2.a(x), -q2.b(x))
  }

  def one[T, @sp(Int,Long,Float,Double) A : Rig](implicit q2: Q2Like[T,A]): T = {
    val ar = implicitly[Rig[A]]
    q2.of(ar.one,ar.zero)
  }

  def times[T, @sp(Int,Long,Float,Double) A : Semiring](x: T, y:T)(implicit q2: Q2Like[T,A]): T = {
    import spire.syntax.semiring._
    val b2s = q2.b(x) * q2.b(y)
    q2.of(q2.a(x) * q2.a(y) + b2s + b2s, q2.a(x) * q2.b(y) + q2.b(x) * q2.a(y))
  }

  def div[T, @sp(Int,Long,Float,Double) A : Field](x: T, y:T)(implicit q2: Q2Like[T,A]): T = {
    val af = implicitly[Field[A]]
    import spire.syntax.field._
    val n = af.reciprocal({
      val b2 = q2.b(y) * q2.b(y)
      q2.a(y) * q2.a(y) - b2 - b2
    })
    val b2s = q2.b(x) * q2.b(y)
    q2.of(n *(q2.a(x) * q2.a(y) - b2s - b2s), n*(q2.b(x) * q2.a(y) - q2.a(x) * q2.b(y)))
  }

  def eqv[T, @sp(Int,Long,Float,Double) A : Eq](x: T, y:T)(implicit q2: Q2Like[T,A]): Boolean = {
    import spire.syntax.eq._
    q2.a(x) === q2.a(y) && q2.b(x) === q2.b(y)
  }

  def neqv[T, @sp(Int,Long,Float,Double) A : Eq](x: T, y:T)(implicit q2: Q2Like[T,A]): Boolean = {
    import spire.syntax.eq._
    q2.a(x) =!= q2.a(y) || q2.b(x) =!= q2.b(y)
  }

  def fromBigInt[T, @sp(Int,Long,Float,Double) A : Ring](n: BigInt)(implicit q2: Q2Like[T,A]): T = {
    val ar = implicitly[Ring[A]]
    q2.of(ar.fromBigInt(n), ar.zero)
  }

  def fromInt[T, @sp(Int,Long,Float,Double) A : Ring](n: Int)(implicit q2: Q2Like[T,A]): T = {
    val ar = implicitly[Ring[A]]
    q2.of(ar.fromInt(n), ar.zero)
  }

  def fromDouble[T, @sp(Int,Long,Float,Double) A : Field](n: Int)(implicit q2: Q2Like[T,A]): T = {
    val af = implicitly[Field[A]]
    q2.of(af.fromDouble(n), af.zero)
  }

  def isOne[T, @sp(Int,Long,Float,Double) A : Rig : Eq](x: T)(implicit q2: Q2Like[T,A]): Boolean = {
    val ar = implicitly[Rig[A]]
    ar.isOne(q2.a(x)) && ar.isZero(q2.b(x))
  }

  def isZero[T, @sp(Int,Long,Float,Double) A : AdditiveMonoid : Eq](x: T)(implicit q2: Q2Like[T,A]): Boolean = {
    val am = implicitly[AdditiveMonoid[A]]
    am.isZero(q2.a(x)) && am.isZero(q2.b(x))
  }

  def sumN[T, @sp(Int,Long,Float,Double) A : AdditiveSemigroup](x: T, n: Int)(implicit q2: Q2Like[T,A]): T = {
    val as = implicitly[AdditiveSemigroup[A]]
    q2.of(as.sumN(q2.a(x),n), as.sumN(q2.b(x),n))
  }
}