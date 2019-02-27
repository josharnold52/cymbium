package cymbium.q2

import algebra.ring.CommutativeRing
import spire.algebra.Signed
import spire.math.{Integral, Natural, SafeLong}

trait RealQuadraticInteger[T,A] extends CommutativeRing[T] with Signed[T] {
  def conjugate(x: T): T
  def signedFieldNorm(x: T): A
  def fundamentalUnit: T

  def discriminant: Natural

  def zpart(x: T): A
  def qpart(x: T): A
  def zq(z: A, q: A): T
}
trait RealQuadraticIntegerQ2[T,A] extends RealQuadraticInteger[T,A] {

  val discriminant = Natural(2)
}
object RealQuadraticIntegerQ2 {

  trait Extension[T, A] extends ExtendToQ2[T, A, Integral]
    with Q2Instances.Q2CommutativeRing[T, A]
    with Signed[T]
    with RealQuadraticIntegerQ2[T,A] {

    override def compare(x: T, y: T): Int = Q2Functions.compare(x, y)

    override def signum(a: T): Int = Q2Functions.signum(a)

    override def abs(a: T): T = Q2Functions.abs(a)

    override def conjugate(x: T): T = Q2Functions.conjugate(x)
    override val fundamentalUnit: T = Q2Functions.fundamentalUnit

    override def signedFieldNorm(x: T): A = Q2Functions.signedFieldNorm(x)

    override def zpart(x: T) = implicitly[Q2Like[T,A]].a(x)
    override def qpart(x: T) = implicitly[Q2Like[T,A]].b(x)

    override def zq(z: A, q: A): T = implicitly[Q2Like[T,A]].of(z,q)

    def euclideanFunction(a: T) = Q2Functions.posFieldNorm(a)
  }

  class StdExtension[T,A](implicit val q2Like: Q2Like[T,A], val baseAlgebraic: Integral[A])  extends Extension[T,A]{

  }

  def main(as: Array[String]): Unit = {
    import Q2.Q2Ref

    import spire.syntax.all._

    implicit val ext = new StdExtension[Q2Ref[SafeLong], SafeLong]

    import ext.q2Like
    import ext.baseAlgebraic

    println(ext.fundamentalUnit)

    println(ext.signedFieldNorm(ext.fundamentalUnit))

    println(ext.zq(14L,-9L).signum)


    val x = ext.zq(14L,22L)
    val y = ext.zq(2L,10L)
    val (q,r) = Q2Functions.equotmodBasicImpl(x,y)
    println(s"$x = $q * $y + $r")
    println(q*y + r)

    println(ext.euclideanFunction(x))
    println(ext.euclideanFunction(q))
    println(ext.euclideanFunction(y))
    println(ext.euclideanFunction(r))
  }


}