package cymbium.q2
import scala.language.experimental.macros
import algebra.Eq
import algebra.ring._
import machinist.DefaultOps
import spire.algebra.{IsReal, NRoot, Signed, TruncatedDivision}
import spire.math.{ConvertableFrom, ConvertableTo, Integral, Rational, Real}

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.{specialized => sp}

trait Q2Like[T, @sp(Int,Long,Float,Double) A] { self =>
  def a(t: T): A
  def b(t: T): A
  def of(a: A, b: A): T
  //private[q2] def cacheBits: Int
  //private[q2] def cacheBits_=(value: Int): Unit
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
    def map[B <: AnyRef](f: A => B): Q2Ref[B] = Q2Ref(f(a),f(b))
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

  //TODO - Should figure out a way to expose a signum cache in Q2Like and use it here?
  // Or do we leave that to the implementers...

  def zero[T, A](implicit q2: Q2Like[T,A], ev: AdditiveMonoid[A]): T = {
    defQ2(ev.zero,ev.zero)
  }

  def plus[T, @sp(Int,Long,Float,Double) A](x: T, y: T)(implicit q2: Q2Like[T,A], ev: AdditiveSemigroup[A]): T = {
    defQ2(x.a + y.a, x.b + y.b)
  }

  def minus[T, @sp(Int,Long,Float,Double) A](x: T, y:T)(implicit q2: Q2Like[T,A], ev: AdditiveGroup[A]): T = {
    defQ2(x.a - y.a, x.b - y.b)
  }

  def negate[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: AdditiveGroup[A]): T = {
    defQ2(-x.a, -x.b)
  }

  def conjugate[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: AdditiveGroup[A]): T = {
    defQ2(x.a, -x.b)
  }
  def one[T, @sp(Int,Long,Float,Double) A](implicit q2: Q2Like[T,A], ev: Rig[A]): T = {
    defQ2(ev.one,ev.zero)
  }
  def fundamentalUnit[T, @sp(Int,Long,Float,Double) A](implicit q2: Q2Like[T,A], ev: Rig[A]): T = {
    defQ2(ev.one,ev.one)
  }

  def times[T, @sp(Int,Long,Float,Double) A](x: T, y:T)(implicit q2: Q2Like[T,A], cr: Semiring[A]): T = {
  //def times[T, @sp(Int,Long,Float,Double) A](x: T, y:T)(implicit cr: Semiring[A], q2: Q2Like[T,A]): T = {
    val bprod = x.b * y.b
    defQ2(x.a * y.a + bprod + bprod, x.a * y.b + x.b * y.a)
  }

  def baseTimesR[T, @sp(Int,Long,Float,Double) A](x: T, y:A)(implicit q2: Q2Like[T,A], ev: Semiring[A]): T = {
    defQ2(x.a * y, x.b * y)
  }
  def baseTimesL[T, @sp(Int,Long,Float,Double) A](x: A, y:T)(implicit q2: Q2Like[T,A], ev: Semiring[A]): T = {
    defQ2(x * y.a, x * y.b)
  }

  def signedFieldNorm[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: CommutativeRng[A]): A = {
    val xbsq = x.b * x.b
    x.a * x.a - xbsq - xbsq
  }

  def reciprocalFraction[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: CommutativeRng[A]): (T,A) = {
    (conjugate(x),signedFieldNorm(x))
  }
  def divFraction[T, @sp(Int,Long,Float,Double) A](x: T, y: T)(implicit q2: Q2Like[T,A], ev: CommutativeRng[A]): (T,A) = {
    (times(x,conjugate(y)),signedFieldNorm(y))
  }

  //def
  def reciprocal[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: Field[A]): T = {
    val (n,d) = reciprocalFraction(x)
    baseTimesR(n,d.reciprocal())
  }

  def div[T, @sp(Int,Long,Float,Double) A](x: T, y:T)(implicit q2: Q2Like[T,A], ev: Field[A]): T = {
    val (n,d) = divFraction(x,y)
    baseTimesR(n,d.reciprocal())
  }

  def posFieldNorm[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: CommutativeRng[A], ev2: Signed[A]): A = {
    ev2.abs(signedFieldNorm(x))
  }


  def eqv[T, @sp(Int,Long,Float,Double) A](x: T, y:T)(implicit q2: Q2Like[T,A], ev: Eq[A]): Boolean = {
    x.a === y.a && x.b === y.b
  }

  def neqv[T, @sp(Int,Long,Float,Double) A](x: T, y:T)(implicit q2: Q2Like[T,A], ev: Eq[A]): Boolean = {
    x.a =!= y.a || x.b =!= y.b
  }

  def fromBigInt[T, @sp(Int,Long,Float,Double) A](n: BigInt)(implicit q2: Q2Like[T,A], ev: Ring[A]): T = {
    defQ2(ev.fromBigInt(n), ev.zero)
  }

  def fromInt[T, @sp(Int,Long,Float,Double) A](n: Int)(implicit q2: Q2Like[T,A], ev: Ring[A]): T = {
    defQ2(ev.fromInt(n), ev.zero)
  }

  def fromDouble[T, @sp(Int,Long,Float,Double) A](n: Int)(implicit q2: Q2Like[T,A], ev: Field[A]): T = {
    defQ2(ev.fromDouble(n), ev.zero)
  }

  def isOne[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: Rig[A], ev2: Eq[A]): Boolean = {
    ev.isOne(x.a) && ev.isZero(x.b)
  }

  def isZero[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: AdditiveMonoid[A], ev2: Eq[A]): Boolean = {
    ev.isZero(x.a) && ev.isZero(x.b)
  }

  def sumN[T, @sp(Int,Long,Float,Double) A](x: T, n: Int)(implicit q2: Q2Like[T,A], ev: AdditiveSemigroup[A]): T = {
    q2.of(ev.sumN(x.a,n), ev.sumN(x.b,n))
  }




  def signum[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: Rng[A], ev2: Signed[A]): Int = {
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

  def compare[T, @sp(Int,Long,Float,Double) A](x: T, y: T)(implicit q2: Q2Like[T,A], ev: Rng[A], ev2: Signed[A]): Int = {
    signum(minus(y,x))
  }

  def abs[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: Rng[A], ev2: Signed[A]): T = {
    if (signum(x) >= 0) x
    else negate(x)
  }

  def isWhole[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: IsReal[A]): Boolean = {
    ev.isSignZero(x.b) && ev.isWhole(x.a)
  }

  def toDouble[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: IsReal[A]): Double = {
    ev.toDouble(x.a) + 1.4142135623730951 *  ev.toDouble(x.b)
  }

  def toReal[T, @sp(Int,Long,Float,Double) A](x: T)(implicit q2: Q2Like[T,A], ev: IsReal[A]): Real = {
    ev.toReal(x.a) + Real(2).nroot(2) *  ev.toReal(x.b)
  }


  /**
    * Returns x/y, rounded to the nearest integer.
    *
    */
  private[q2] def roundQuot[@sp(Int,Long,Float,Double) A : TruncatedDivision : CommutativeRing](x: A, y: A): (A) = {
    //x / y

    val ys = y.signum()
    require(ys != 0)
    val (lower,r) = x.fquotmod(y)
    val chk = y.compare(r + r)

    val roundUp = (chk < 0) ^ (ys < 0)

    if (roundUp) lower + implicitly[CommutativeRing[A]].one else lower
  }

  def equotmod[T, @sp(Int,Long,Float,Double) A](x: T, y: T)(implicit q2: Q2Like[T,A], ev: TruncatedDivision[A], ev2: CommutativeRing[A]): (T,T) = {
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
    val (en,ed) = divFraction(x,y)
    val q = defQ2(roundQuot(en.a,ed), roundQuot(en.b,ed))
    val r = minus(x,times(q,y))
    (q,r)
  }

  def equot[T, @sp(Int,Long,Float,Double) A](x: T, y: T)(implicit q2: Q2Like[T,A], ev: TruncatedDivision[A], ev2: CommutativeRing[A]): T = {
    val (en,ed) = divFraction(x,y)
    val q = defQ2(roundQuot(en.a,ed), roundQuot(en.b,ed))
    q
  }
  def emod[T, @sp(Int,Long,Float,Double) A](x: T, y: T)(implicit q2: Q2Like[T,A], ev: TruncatedDivision[A], ev2: CommutativeRing[A]): T = {
    equotmod(x,y)._2
  }


  def gcd[T, @sp(Int,Long,Float,Double) A](x: T, y: T)(implicit q2: Q2Like[T,A], ev: TruncatedDivision[A], ev2: CommutativeRing[A]): T = {
    import spire.syntax.ring._
    if (isZero(x)) {
      return y
    }
    @tailrec
    def gcd0(y1: T, y2: T): T = {
      if (isZero(y2)) y1
      else {
        gcd0(y2, emod(y1,y2))
      }
    }
    gcd0(x,y)
  }

  def lcm[T, @sp(Int,Long,Float,Double) A](x: T, y: T)(implicit q2: Q2Like[T,A], ev: TruncatedDivision[A], ev2: CommutativeRing[A]): T = {
    val (n,d) = divFraction(times(x,y),gcd(x,y))
    defQ2(ev.fquot(n.a,d),ev.fquot(n.b,d))
  }

  //def q2[T, @sp(Int,Long,Float,Double) A](x: A, y: A)(implicit q2: Q2Like[T,A]): A =
  //private final val doubler2 = 1.4142135623730951
  //private val rsq2 =
}