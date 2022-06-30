package cymbium.newq2

import algebra.Eq
import algebra.ring.{AdditiveGroup, AdditiveMonoid, AdditiveSemigroup}

import scala.language.higherKinds

trait LinearReprFunctions[L[S,A] <: LinearRepr[S,A]] {

  import syntax.linearRepr._
  import spire.syntax.all._

  def plus[S, A](x: S, y: S)(implicit q2: L[S,A], ev: AdditiveSemigroup[A]): S = {
    lrepr(x.t0 + y.t0, x.t1 + y.t1)
  }

  def sumN[S, A](a: S, n: Int)(implicit q2: L[S,A], ev: AdditiveSemigroup[A]): S = {
    //Not sure if this is worth it.... What's more costly?  Unecessary lrepr calls
    //or the extra loop overhead of doing sumN twice.
    lrepr(ev.sumN(a.t0, n), ev.sumN(a.t1,n))
  }
  def zero[S, A](implicit q2: L[S,A], ev: AdditiveMonoid[A]): S = {
    val z = ev.zero
    lrepr(z,z)
  }

  def minus[S, A](x: S, y:S)(implicit q2: L[S,A], ev: AdditiveGroup[A]): S = {
    lrepr(x.t0 - y.t0, x.t1 - y.t1)
  }

  def negate[S, A](x: S)(implicit q2: L[S,A], ev: AdditiveGroup[A]): S = {
    lrepr(-x.t0, -x.t1)
  }

  def isZero[S, A](x: S)(implicit q2: L[S,A], ev: AdditiveMonoid[A], ev2: Eq[A]): Boolean = {
    ev.isZero(x.t0) && ev.isZero(x.t0)
  }

  def eqv[S,A](x: S, y: S)(implicit q2: L[S,A], ev: Eq[A]): Boolean = {
    ev.eqv(x.t0,y.t0) && ev.eqv(x.t1,y.t1)
  }
  def neqv[S,A](x: S, y: S)(implicit q2: L[S,A], ev: Eq[A]): Boolean = {
    ev.neqv(x.t0,y.t0) || ev.neqv(x.t1,y.t1)
  }

  /**
    * Standard inclusion homomorphism
    */
  def lift[A,T](x: A)(implicit q2: L[T,A], ev: AdditiveMonoid[A]): T = {
    lrepr(x,ev.zero)
  }

  def liftInv[T, A](s: T)(implicit q2: L[T,A], ev: AdditiveMonoid[A], eq: Eq[A]): Option[A] = {
    if (ev.isZero(s.t1)) Some(s.t0) else None
  }
  def liftInvPf[T, A](implicit q2: L[T,A], ev: AdditiveMonoid[A], eqe: Eq[A]): PartialFunction[T,A] = new PartialFunction[T,A] {
    //if (ev.isZero(s.t1)) Some(s.t0) else None
    override def apply(x: T): A = x.t0
    override def isDefinedAt(x: T): Boolean = ev.isZero(x.t1)
  }

}
