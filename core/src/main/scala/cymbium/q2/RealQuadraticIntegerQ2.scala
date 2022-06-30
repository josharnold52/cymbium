package cymbium.q2

import algebra.ring.CommutativeRing
import spire.algebra._
import spire.math.{Integral, Natural, Rational, SafeLong}

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
  trait Extension[T, A] extends Extend2ToQ2[T, A, CommutativeRing, TruncatedDivision]
    with Q2Instances.Q2CommutativeRing[T, A]
    with Signed[T]
    with RealQuadraticIntegerQ2[T,A]
    with CModule[T,A]
    with EuclideanRing[T] {

    private var signnumCache: Int = _

    override def compare(x: T, y: T): Int = Q2Functions.compare(x, y)

    override def signum(a: T): Int = {
      val x = signnumCache
      if (x != 0) x-2
      else {
        val y = Q2Functions.signum(a)
        signnumCache = y + 2
        y
      }
    }

    override def abs(a: T): T = Q2Functions.abs(a)

    override def conjugate(x: T): T = Q2Functions.conjugate(x)
    override val fundamentalUnit: T = Q2Functions.fundamentalUnit

    override def signedFieldNorm(x: T): A = Q2Functions.signedFieldNorm(x)

    override def zpart(x: T) = implicitly[Q2Like[T,A]].a(x)
    override def qpart(x: T) = implicitly[Q2Like[T,A]].b(x)

    override def zq(z: A, q: A): T = implicitly[Q2Like[T,A]].of(z,q)

    val baseConversion: IsIntegral[A]

    def euclideanFunction(a: T): BigInt = {
      val n = Q2Functions.posFieldNorm(a)
      baseConversion.toBigInt(n)
    }

    def timesl(r: A, t:T): T = Q2Functions.baseTimesL(r,t)

    def scalar = baseAlgebraic

    override def gcd(a: T, b: T)(implicit ev: Eq[T]): T = Q2Functions.gcd(a,b)

    override def equot(a: T, b: T): T = Q2Functions.equot(a,b)

    override def emod(a: T, b: T): T = Q2Functions.emod(a,b)

    override def equotmod(a: T, b: T): (T, T) = Q2Functions.equotmod(a,b)

    override def lcm(a: T, b: T)(implicit ev: Eq[T]) = Q2Functions.lcm(a,b)
  }

  class StdExtension[T,A](implicit val q2Like: Q2Like[T,A], val baseAlgebraic: CommutativeRing[A],
                          val baseOrder: TruncatedDivision[A],
                          val baseConversion: IsIntegral[A])  extends Extension[T,A] {

  }

  def xmain(as: Array[String]): Unit = {
    import Q2.Q2Ref

    import spire.syntax.all._

    import SafeLong.SafeLongIsReal

    implicit val ext = new StdExtension[Q2Ref[SafeLong], SafeLong]

    //implicit val sl = SafeLong.

    import ext.q2Like
    import ext.baseAlgebraic

    //println(ext.fundamentalUnit)

    //println(ext.signedFieldNorm(ext.fundamentalUnit))

    //println(ext.zq(14L,-9L).signum)

    val rnd = new scala.util.Random(System.currentTimeMillis())
    def pickLong() = rnd.nextInt(10000).toLong

    val x = ext.zq(pickLong(),pickLong())
    val y = ext.zq(pickLong()/10,pickLong()/10)

    println(s"x = $x")
    println(s"y = $y")

    val (q,r) = Q2Functions.equotmod(x,y)
    println("Euclidean Division: ")
    println(s"x/y = $x / $y = $q with remainder of $r")
    println(s"$x = $q * $y + $r = ${q*y + r}")

    println("|x|="+ext.euclideanFunction(x))
    println("|y|="+ext.euclideanFunction(y))
    println("|q|="+ext.euclideanFunction(q))
    println("|r|="+ext.euclideanFunction(r))

    println("Rational Division: ");
    {
      import Rational.RationalAlgebra
      val xr = x.map{_.toRational()}
      val yr = y.map{_.toRational()}
      println(Q2Functions.signedFieldNorm(yr))
      println(Q2Functions.divFraction(xr,yr))
      val qr = Q2Functions.div(xr,yr)

      val xxr:Q2Ref[Rational] = Q2Functions.times(qr,yr)
      println(s"$xr / $yr = $qr")
      println(s"$qr * $yr = $xxr")
    }

    println("GCD: ")
    val gcd = x.gcd(y)
    //val gcd = Utilities.gcd(x,y)((a,b) => Q2Functions.equotmod(a,b))
    println(s"GCD = $gcd")
    println(s"$x / $gcd = ${Q2Functions.divFraction(x,gcd)}")
    println(s"$y / $gcd = ${Q2Functions.divFraction(y,gcd)}")
  }


}