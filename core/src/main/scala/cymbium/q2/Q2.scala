package cymbium.q2
import scala.language.experimental.macros
import algebra.Eq
import algebra.ring._
import machinist.DefaultOps
import spire.algebra.{IsReal, NRoot, Signed}
import spire.math.{ConvertableFrom, ConvertableTo, Rational, Real, Integral}

import scala.language.higherKinds
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

  object syntax {
    implicit class Q2LikeOps[T,A](x: T)(implicit ev: Q2Like[T,A]) {
      def a: A = macro DefaultOps.unop0[A]
      def b: A = macro DefaultOps.unop0[A]
    }
    //implicit def ops[T,A](x: T)(implicit ev: Q2Like[T,A]): Q2LikeOps[T,A] = new Q2LikeOps(x)
    @inline
    implicit def defQ2[T,A](a1: A, a2: A)(implicit ev: Q2Like[T,A]): T =
      macro cymbium.macros.DefaultForwarders.forwardOf2[A,A, Q2Like[T,A], T] //ev.of(x,y)
  }
}

trait Q2[@sp(Int,Long,Float,Double) A] {
  def a: A
  def b: A
  override def toString() = s"$a+$b\u221A2"
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
  import Q2Like.syntax._
  import spire.syntax.all._

  def zero[T, A : AdditiveMonoid](implicit q2: Q2Like[T,A]): T = {
    val am = implicitly[AdditiveMonoid[A]]

    defQ2(am.zero,am.zero)
  }

  def plus[T, @sp(Int,Long,Float,Double) A : AdditiveSemigroup](x: T, y: T)(implicit q2: Q2Like[T,A]): T = {
    defQ2(x.a + y.a, x.b + y.b)
  }

  def minus[T, @sp(Int,Long,Float,Double) A : AdditiveGroup](x: T, y:T)(implicit q2: Q2Like[T,A]): T = {
    defQ2(x.a - y.a, x.b - y.b)
  }

  def negate[T, @sp(Int,Long,Float,Double) A : AdditiveGroup](x: T)(implicit q2: Q2Like[T,A]): T = {
    defQ2(-x.a, -x.b)
  }

  def conjugate[T, @sp(Int,Long,Float,Double) A : AdditiveGroup](x: T)(implicit q2: Q2Like[T,A]): T = {
    defQ2(x.a, -x.b)
  }
  def one[T, @sp(Int,Long,Float,Double) A : Rig](implicit q2: Q2Like[T,A]): T = {
    val ar = implicitly[Rig[A]]
    defQ2(ar.one,ar.zero)
  }
  def fundamentalUnit[T, @sp(Int,Long,Float,Double) A : Rig](implicit q2: Q2Like[T,A]): T = {
    val ar = implicitly[Rig[A]]
    defQ2(ar.one,ar.one)
  }

  def times[T, @sp(Int,Long,Float,Double) A : Semiring](x: T, y:T)(implicit q2: Q2Like[T,A]): T = {
    val bprod = x.b * y.b
    defQ2(x.a * y.a + bprod + bprod, x.a * y.b + x.b * y.a)
  }

  def signedFieldNorm[T, @sp(Int,Long,Float,Double) A : CommutativeRng](x: T)(implicit q2: Q2Like[T,A]): A = {
    val xbsq = x.b * x.b
    x.a * x.a - xbsq - xbsq
  }

  def posFieldNorm[T, @sp(Int,Long,Float,Double) A : CommutativeRng : Signed](x: T)(implicit q2: Q2Like[T,A]): A = {
    val xbsq = x.b * x.b
    implicitly[Signed[A]].abs(x.a * x.a - xbsq - xbsq)
  }

  def div[T, @sp(Int,Long,Float,Double) A : Field](x: T, y:T)(implicit q2: Q2Like[T,A]): T = {
    val af = implicitly[Field[A]]

    val n = af.reciprocal({
      val ybsq = y.b * y.b
      y.a * y.a - ybsq - ybsq
    })
    val bprod = x.b * y.b
    val ra = x.a * y.a - bprod - bprod
    val rb = x.b * y.a - x.a * y.b

    defQ2(n * ra , n * rb)
  }

  def eqv[T, @sp(Int,Long,Float,Double) A : Eq](x: T, y:T)(implicit q2: Q2Like[T,A]): Boolean = {
    x.a === y.a && x.b === y.b
  }

  def neqv[T, @sp(Int,Long,Float,Double) A : Eq](x: T, y:T)(implicit q2: Q2Like[T,A]): Boolean = {
    x.a =!= y.a || x.b =!= y.b
  }

  def fromBigInt[T, @sp(Int,Long,Float,Double) A : Ring](n: BigInt)(implicit q2: Q2Like[T,A]): T = {
    val ar = implicitly[Ring[A]]
    defQ2(ar.fromBigInt(n), ar.zero)
  }

  def fromInt[T, @sp(Int,Long,Float,Double) A : Ring](n: Int)(implicit q2: Q2Like[T,A]): T = {
    val ar = implicitly[Ring[A]]
    defQ2(ar.fromInt(n), ar.zero)
  }

  def fromDouble[T, @sp(Int,Long,Float,Double) A : Field](n: Int)(implicit q2: Q2Like[T,A]): T = {
    val af = implicitly[Field[A]]
    defQ2(af.fromDouble(n), af.zero)
  }

  def isOne[T, @sp(Int,Long,Float,Double) A : Rig : Eq](x: T)(implicit q2: Q2Like[T,A]): Boolean = {
    val ar = implicitly[Rig[A]]
    ar.isOne(x.a) && ar.isZero(x.b)
  }

  def isZero[T, @sp(Int,Long,Float,Double) A : AdditiveMonoid : Eq](x: T)(implicit q2: Q2Like[T,A]): Boolean = {
    val am = implicitly[AdditiveMonoid[A]]
    am.isZero(x.a) && am.isZero(x.b)
  }

  def sumN[T, @sp(Int,Long,Float,Double) A : AdditiveSemigroup](x: T, n: Int)(implicit q2: Q2Like[T,A]): T = {
    val as = implicitly[AdditiveSemigroup[A]]
    q2.of(as.sumN(x.a,n), as.sumN(x.b,n))
  }



  def compare[T, @sp(Int,Long,Float,Double) A : Rng : Signed](x: T, y: T)(implicit q2: Q2Like[T,A]): Int = {
    val a = x.a - y.a
    val b = y.b - x.a

    val as = a.signum()
    val bs = b.signum()

    if (bs == 0) as
    else if (as != bs) -bs
    else {
      val asq = a * a
      val bsq = b * b
      val c = (asq - bsq).compare(bsq)
      if (as > 0) c
      else if (c < 0) 1
      else -c
    }
  }

  def signum[T, @sp(Int,Long,Float,Double) A : Rng : Signed](x: T)(implicit q2: Q2Like[T,A]): Int = {
    val a = x.a
    val b = x.b

    val as = a.signum()
    val bs = b.signum()

    if (as == 0 || bs == 0 || as == bs) as | bs
    else {
      val asq = a * a
      val bsq = b * b
      val diff = asq - bsq
      val cmp = diff.compare(bsq)
      val s = if (cmp == 0) 0 else if (cmp < 0) -1 else 1
      if (as > 0) s else -s
    }
  }
  def abs[T, @sp(Int,Long,Float,Double) A : Rng : Signed](x: T)(implicit q2: Q2Like[T,A]): T = {
    if (signum(x) >= 0) x
    else negate(x)
  }

  def isWhole[T, @sp(Int,Long,Float,Double) A : IsReal](x: T)(implicit q2: Q2Like[T,A]): Boolean = {
    val ev = implicitly[IsReal[A]]
    ev.isSignZero(x.b) && ev.isWhole(x.a)
  }

  def toDouble[T, @sp(Int,Long,Float,Double) A : IsReal](x: T)(implicit q2: Q2Like[T,A]): Double = {
    //x.a.toDouble()
    val ev = implicitly[IsReal[A]]
    ev.toDouble(x.a) + 1.4142135623730951 *  ev.toDouble(x.b)
  }

  def toReal[T, @sp(Int,Long,Float,Double) A : IsReal](x: T)(implicit q2: Q2Like[T,A]): Real = {
    //x.a.toDouble()
    val ev = implicitly[IsReal[A]]
    ev.toReal(x.a) + Real(2).nroot(2) *  ev.toReal(x.b)
  }

  def equotmodBasicImpl[T, @sp(Int,Long,Float,Double) A : Integral](x: T, y: T)(implicit q2: Q2Like[T,A]): (T,T) = {
    val ev = implicitly[Integral[A]]

    if (isZero(y)) throw new ArithmeticException()

    val xr = Q2(x.a.toRational(),x.b.toRational())
    val yr = Q2(y.a.toRational(),y.b.toRational())
    import Rational.RationalAlgebra
    val er = div(xr,yr)

    val q = defQ2(ev.fromRational(er.a.round), ev.fromRational(er.b.round))
    val qy = times(q,y)(ev,q2)
    val r = minus(x, qy)(ev,q2)
    (q,r)
  }

  def equotmod[T, @sp(Int,Long,Float,Double) A : Integral](x: T, y: T)(implicit q2: Q2Like[T,A]): (T,T) = {
    //Euclidean quotient/mod uperation using the "absFieldnorm"

    //Goal: given x,y Find q,r s.t   x = yq + r  and absFieldNorm(r) < absFieldNorm(y)

    //Basic insight:  We can actually extends absFieldNorm to the field of fractions in the obvious way.
    // (in which case the range of absFieldNorm becomes the non-negative rationals).
    //  If we do this, it suffices to solve the goal for y=1.  Why?  Because the multiplicative property
    //  implies that if (q,r) is a solution for (x,y) then (q,ar) is a solution for (ax,ay).  And in the field
    //  of fractions, we can always scale to y = 1.
    //  So assume y=1. Let q be z2(round(x.a),round(x.b)) and r = x - q.   Then abs(r.a) <= 0.5 and abs(r.b) <= 0.5
    //   so absFieldNorm(r) = abs(r.a^2 - 2*r.b^2) <= 0.25 + 2 * 0.25 < 1, which was to be shown.
    //
    //  So... For general x,y, Let e = x/y (symbolically).  Then take q to be z2(round(e.a),round(e.b)).  Then
    //  r is just x - qy  (or y(e-q), whichever is easier).
    //

    ???
  }

  //def q2[T, @sp(Int,Long,Float,Double) A](x: A, y: A)(implicit q2: Q2Like[T,A]): A =

  //private final val doubler2 = 1.4142135623730951
  //private val rsq2 =
}