package cymbium.newq2

import algebra.Eq
import algebra.ring._
import machinist.DefaultOps

import scala.language.higherKinds
import scala.language.experimental.macros

trait LinearRepr[S,A] {
  def t0(s: S): A
  def t1(s: S): A

  def of(t0: A, t1: A): S

}


object LinearRepr extends LinearReprFunctions[LinearRepr] {

  type GenLinearRepr[S[_],A] = LinearRepr[S[A],A]

  class Ops[S,A](x: S)(implicit ev: LinearRepr[S,A]) {
    def t0: A = macro DefaultOps.unop0[A]
    def t1: A = macro DefaultOps.unop0[A]
  }

  class TupleLinearRepr[A] extends LinearRepr[(A,A),A] { self =>
    def t0(s: (A,A)):A = s._1
    def t1(s: (A,A)):A = s._2
    def of(t0: A, t1: A): (A,A) = (t0,t1)


  }



  case class LRep[+A](t0: A, t1: A)
  object LRep extends LinearReprInstances[LRep] {
    class LinearReprForLRep[A] extends LinearRepr[LRep[A],A] { self =>
      def t0(s: LRep[A]):A = s.t0
      def t1(s: LRep[A]):A = s.t1
      def of(t0: A, t1: A): LRep[A] = LRep(t0,t1)
    }
    implicit def linearReprForLRep[A]: GenLinearRepr[LRep,A] = new LinearReprForLRep[A]

    override protected[this] def linearRepr[A] = linearReprForLRep[A]
  }

}


trait LinearReprInstances[S[_]] {
  import LinearRepr._
  protected[this] implicit def linearRepr[A]: LinearRepr[S[A],A]

  implicit def eqForLinearRepr[A](implicit ev: Eq[A]): Eq[S[A]] = LinearReprInstances.eqForLinearRepr[S[A],A]

  implicit def additiveSemigroupForLinearRepr[A](implicit ev: AdditiveSemigroup[A]): AdditiveSemigroup[S[A]] =
    LinearReprInstances.additiveSemigroupForLinearRepr[S[A],A]
  implicit def additiveMonoidForLinearRepr[A](implicit ev: AdditiveMonoid[A]): AdditiveMonoid[S[A]] =
    LinearReprInstances.additiveMonoidForLinearRepr[S[A],A]
  implicit def additiveGroupForLinearRepr[A](implicit ev: AdditiveGroup[A]): AdditiveGroup[S[A]] =
    LinearReprInstances.additiveGroupForLinearRepr[S[A],A]
  implicit def additiveCommutativeSemigroupForLinearRepr[A](implicit ev: AdditiveCommutativeSemigroup[A]): AdditiveCommutativeSemigroup[S[A]] =
    LinearReprInstances.additiveCommutativeSemigroupForLinearRepr[S[A],A]
  implicit def additiveCommutativeMonoidForLinearRepr[A](implicit ev: AdditiveCommutativeMonoid[A]): AdditiveCommutativeMonoid[S[A]] =
    LinearReprInstances.additiveCommutativeMonoidForLinearRepr[S[A],A]
  implicit def additiveCommutativeGroupForLinearRepr[A](implicit ev: AdditiveCommutativeGroup[A]): AdditiveCommutativeGroup[S[A]] =
    LinearReprInstances.additiveCommutativeGroupForLinearRepr[S[A],A]
}

object LinearReprInstances {


  trait EqLike[+T[_],S,A] {
    private[LinearReprInstances] implicit def eqLike: T[A]
    private[LinearReprInstances] implicit def lrepr: LinearRepr[S,A]
  }
  abstract class EqLikeImpl[T[_],S,A](implicit e: T[A], lr: LinearRepr[S,A]) extends EqLike[T,S,A] {
    override private[LinearReprInstances] implicit def eqLike: T[A] = e
    override private[LinearReprInstances] implicit def lrepr: LinearRepr[S,A] = lr
  }

  trait EqForLinearRepr[S,A] extends Eq[S] with EqLike[Eq,S,A] {
    override def eqv(x: S, y: S): Boolean = LinearRepr.eqv(x,y)
    override def neqv(x: S, y: S): Boolean = LinearRepr.neqv(x,y)
  }

  implicit def eqForLinearRepr[S,A](implicit ev: Eq[A], lr: LinearRepr[S,A]): Eq[S] =
    new EqLikeImpl[Eq,S,A] with EqForLinearRepr[S,A]


  trait AdditiveLike[+T[_],S,A] {
    private[LinearReprInstances] implicit def addLike: T[A]
    private[LinearReprInstances] implicit def lrepr: LinearRepr[S,A]
  }
  abstract class AdditiveLikeImpl[T[_],S,A](implicit e: T[A], lr: LinearRepr[S,A]) extends AdditiveLike[T,S,A] {
    override private[LinearReprInstances] implicit def addLike: T[A] = e
    override private[LinearReprInstances] implicit def lrepr: LinearRepr[S,A] = lr
  }

  trait EqAdditiveLike[+EL[_],+AL[_],S,A] extends EqLike[EL,S,A] with AdditiveLike[AL,S,A]

  abstract class EqAdditiveLikeImpl[EL[_],AL[_],S,A](implicit el: EL[A], al: AL[A], lr: LinearRepr[S,A])
      extends EqAdditiveLike[EL,AL,S,A] {
    override private[LinearReprInstances] implicit def eqLike: EL[A] = el
    override private[LinearReprInstances] implicit def addLike: AL[A] = al
    override private[LinearReprInstances] implicit def lrepr: LinearRepr[S,A] = lr
  }



  trait AdditiveSemigroupForLinearRepr[S,A] extends AdditiveSemigroup[S] with AdditiveLike[AdditiveSemigroup,S,A] {
    override def plus(x: S, y: S): S = LinearRepr.plus(x,y)
    override def sumN(a: S, n: Int): S = LinearRepr.sumN(a,n)
  }

  trait AdditiveMonoidForLinearRepr[S,A] extends AdditiveMonoid[S] with AdditiveLike[AdditiveMonoid,S,A]
    with AdditiveSemigroupForLinearRepr[S,A] {
    //TODO - Do we prefer val or def here?
    override val zero: S = LinearRepr.zero[S,A]
    //TODO - If we could summon an Eq[A] , we could override isZero to avoid calling zero
    // ( in case we wanted a def).
  }

  trait AdditiveGroupForLinearRepr[S,A] extends AdditiveGroup[S] with AdditiveLike[AdditiveGroup,S,A]
    with AdditiveMonoidForLinearRepr[S,A] {
    override def minus(x: S, y: S): S = LinearRepr.minus(x,y)
    override def negate(x: S): S = LinearRepr.negate(x)
  }


  trait AdditiveCommutativeSemigroupForLinearRepr[S,A] extends AdditiveCommutativeSemigroup[S] with
    AdditiveLike[AdditiveCommutativeSemigroup,S,A] with AdditiveSemigroupForLinearRepr[S,A]
  
  trait AdditiveCommutativeMonoidForLinearRepr[S,A] extends AdditiveCommutativeMonoid[S] with
    AdditiveLike[AdditiveCommutativeMonoid,S,A] with AdditiveMonoidForLinearRepr[S,A]

  trait AdditiveCommutativeGroupForLinearRepr[S,A] extends AdditiveCommutativeGroup[S] with
    AdditiveLike[AdditiveCommutativeGroup,S,A] with AdditiveGroupForLinearRepr[S,A]


  trait RingLike[+T[_],S,A] extends AdditiveLike[T,S,A] {
    private[LinearReprInstances] implicit def addLike: T[A]
    private[LinearReprInstances] implicit def lrepr: SqReducible[S,A]
  }
  abstract class RingLikeImpl[T[_],S,A](implicit e: T[A], lr: SqReducible[S,A]) extends RingLike[T,S,A] {
    override private[LinearReprInstances] implicit def addLike: T[A] = e
    override private[LinearReprInstances] implicit def lrepr: SqReducible[S,A] = lr
  }

  trait SemiringForSqReducible[S,A] extends Semiring[S]
    with AdditiveCommutativeMonoidForLinearRepr[S,A]
    with RingLike[Semiring,S,A] {
    override def times(x: S, y: S): S = SqReducible.times(x,y)
  }


  implicit def additiveSemigroupForLinearRepr[S,A](implicit e: AdditiveSemigroup[A],
                     lr: LinearRepr[S,A]): AdditiveSemigroup[S] =
    new AdditiveLikeImpl[AdditiveSemigroup,S,A] with AdditiveSemigroupForLinearRepr[S,A]

  implicit def additiveMonoidForLinearRepr[S,A](implicit e: AdditiveMonoid[A],
                                                   lr: LinearRepr[S,A]): AdditiveMonoid[S] =
    new AdditiveLikeImpl[AdditiveMonoid,S,A] with AdditiveMonoidForLinearRepr[S,A]

  implicit def additiveGroupForLinearRepr[S,A](implicit e: AdditiveGroup[A],
                                                lr: LinearRepr[S,A]): AdditiveGroup[S] =
    new AdditiveLikeImpl[AdditiveGroup,S,A] with AdditiveGroupForLinearRepr[S,A]

  implicit def additiveCommutativeSemigroupForLinearRepr[S,A](implicit e: AdditiveCommutativeSemigroup[A],
                                                   lr: LinearRepr[S,A]): AdditiveCommutativeSemigroup[S] =
    new AdditiveLikeImpl[AdditiveCommutativeSemigroup,S,A] with AdditiveCommutativeSemigroupForLinearRepr[S,A]

  implicit def additiveCommutativeMonoidForLinearRepr[S,A](implicit e: AdditiveCommutativeMonoid[A],
                                                lr: LinearRepr[S,A]): AdditiveCommutativeMonoid[S] =
    new AdditiveLikeImpl[AdditiveCommutativeMonoid,S,A] with AdditiveCommutativeMonoidForLinearRepr[S,A]

  implicit def additiveCommutativeGroupForLinearRepr[S,A](implicit e: AdditiveCommutativeGroup[A],
                                               lr: LinearRepr[S,A]): AdditiveCommutativeGroup[S] =
    new AdditiveLikeImpl[AdditiveCommutativeGroup,S,A] with AdditiveCommutativeGroupForLinearRepr[S,A]
  //class

  //implicit def multSgxxx[S,A](implicit sg: MultiplicativeSemigroup[A], lr: LinearRepr[S,A])
  implicit def semiringForSqReducible[S,A](implicit e: Semiring[A],
                                           sq: SqReducible[S,A]): Semiring[S] =
    new RingLikeImpl[Semiring,S,A] with SemiringForSqReducible[S,A]

}
