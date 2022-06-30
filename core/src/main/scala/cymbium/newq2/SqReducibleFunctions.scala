package cymbium.newq2

import algebra.ring._
import spire.algebra.Eq
import scala.language.higherKinds


trait SqReducibleFunctions[L[S,A] <: SqReducible[S,A]] extends LinearReprFunctions[L] {
  import syntax.linearRepr._
  import spire.syntax.all._

  //TODO: Will need tests for all of these.  Its too easy to make a mistake with the
  //order of arguments in the lrepr function...

  /*
   * Note regarding non-commutative base rings:  We assume that we're extending the base ring
   * with an element x such that x^2 is equal to the d1sq element of the SqReducible type
   * and such that x commutes with all elements of the base ring.  This requires that d1sq.t0
   * and d1s2.t1 both lie in the center of the base ring.  ( Proof: Given a, we have
   * a * x^2 == x^2 * a.  Substitute d1sq for x^2 and you get similar relationships for
   * both coefficients of d1sq).
   *
   * If the d1sq requirement is not satisfied, I suspect "times" will fail to have key properties
   * like associativity, but I haven't worked out any explicit counter-examples.
   */

  def times[T, A](x: T, y:T)(implicit sr: L[T,A], cr: Semiring[A]): T = {
    val p2 = x.t1 * y.t1
    val d1sq = sr.d1sq
    lrepr(p2 * d1sq.t0 + x.t0 * y.t0, p2 * d1sq.t1 + x.t1 * y.t0 + x.t0 * y.t1)
  }
  def one[T, A](implicit sr: L[T,A], ev: Rig[A]): T = {
    lrepr(ev.one,ev.zero)
  }
  def baseTimesR[T, A](x: T, y:A)(implicit sr: L[T,A], ev: Semiring[A]): T = {
    lrepr(x.t0 * y, x.t1 * y)
  }
  def baseTimesL[T, A](x: A, y:T)(implicit sr: L[T,A], ev: Semiring[A]): T = {
    lrepr(x * y.t0, x * y.t1)
  }


  /**
    * TODO: Check that I have this correct.   (Based on sum of the two roots of
    * a monic quadratic polynomial is -b, where b is the corefficient of the linear term).
    *
    * A good sanity check is to verify that this defines a ring homomorphism
    */
  def conjugate[T, A](x: T)(implicit sr: L[T,A], ev: Rng[A]): T = {
    //See notes about non-commutativity, above.  In particular, we assume
    //d1sq.t1 is in the center of the base ring so order of multiplication doesn't matter
    lrepr(sr.d1sq.t1 * x.t1  + x.t0,  - x.t1)
  }


  /**
    * This is `x * conjugate(x)`.  For a galois extension, this is the norm of the extension.
    *
    * TODO: When is this a multiplicitive morphism?
    */
  def n[T, A](x: T)(implicit sr: L[T,A], ev: Rng[A]): A = {
    val d1sq = sr.d1sq
    x.t0 * (x.t0 + d1sq.t1) - x.t1 * x.t1 * d1sq.t0
  }

  /**
    * This is just `x + conjugate(x)`.  For a galois extension, this is the trace of the extension.
    *
    * TODO: Is this always an additive morphism?
    */
  def tr[T,A](x: T)(implicit sr: L[T,A], ev: Semiring[A]): A = {
    sr.d1sq.t1 * x.t1  + x.t0 + x.t0
  }
}
