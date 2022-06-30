package cymbium.generic

import algebra.ring.{AdditiveCommutativeMonoid, AdditiveCommutativeSemigroup, MultiplicativeCommutativeMonoid, MultiplicativeCommutativeSemigroup}
import algebra.{CommutativeMonoid, CommutativeSemigroup}
import cats.Semigroup
import spire.algebra._

sealed trait WithUnit[P] {
  def combine(y: WithUnit[P])(implicit sg: Semigroup[P]): WithUnit[P]
  def combineN(n: Int)(implicit sg: Semigroup[P]): WithUnit[P]
  def plus(y: WithUnit[P])(implicit sg: AdditiveSemigroup[P]): WithUnit[P]
  def sumN(n: Int)(implicit sg: AdditiveSemigroup[P]): WithUnit[P]
  def times(y: WithUnit[P])(implicit sg: MultiplicativeSemigroup[P]): WithUnit[P]
  def pow(n: Int)(implicit sg: MultiplicativeSemigroup[P]): WithUnit[P]
}

object WithUnit {
  def apply[P](p: P): Base[P] = Base(p)
  def unit[P]: Adjoined[P] = Adjoined[P]


  case class Base[P](p: P) extends WithUnit[P] {
    override def combine(y: WithUnit[P])(implicit sg: Semigroup[P]): WithUnit[P] = y match {
      case Base(by) => Base(sg.combine(p,by))
      case _ => this
    }

    override def combineN(n: Int)(implicit sg: Semigroup[P]): WithUnit[P] = {
      require(n >= 0)
      if (n == 0) Adjoined[P] else Base(sg.combineN(p, n))
    }
    override def plus(y: WithUnit[P])(implicit sg: AdditiveSemigroup[P]): WithUnit[P] = y match {
      case Base(by) => Base(sg.plus(p,by))
      case _ => this
    }

    override def sumN(n: Int)(implicit sg: AdditiveSemigroup[P]): WithUnit[P] = {
      require(n >= 0)
      if (n == 0) Adjoined[P] else Base(sg.sumN(p, n))
    }
    override def times(y: WithUnit[P])(implicit sg: MultiplicativeSemigroup[P]): WithUnit[P] = y match {
      case Base(by) => Base(sg.times(p,by))
      case _ => this
    }

    override def pow(n: Int)(implicit sg: MultiplicativeSemigroup[P]): WithUnit[P] = {
      require(n >= 0)
      if (n == 0) Adjoined[P] else Base(sg.pow(p, n))
    }

  }

  class Adjoined[P] extends WithUnit[P] {
    override def equals(obj: Any): Boolean = obj.isInstanceOf[Adjoined[_]]
    override def hashCode(): Int = 103043
    override def toString() = "Adjoined"

    override def combine(y: WithUnit[P])(implicit sg: Semigroup[P]): WithUnit[P] = y

    override def combineN(n: Int)(implicit sg: Semigroup[P]): WithUnit[P] = {
      require(n >= 0)
      this
    }
    override def plus(y: WithUnit[P])(implicit sg: AdditiveSemigroup[P]): WithUnit[P] = y

    override def sumN(n: Int)(implicit sg: AdditiveSemigroup[P]): WithUnit[P] = {
      require(n >= 0)
      this
    }
    override def times(y: WithUnit[P])(implicit sg: MultiplicativeSemigroup[P]): WithUnit[P] = y

    override def pow(n: Int)(implicit sg: MultiplicativeSemigroup[P]): WithUnit[P] = {
      require(n >= 0)
      this
    }
  }

  object Adjoined {
    private val theAdjoined = new Adjoined[Nothing]
    def apply[P]: Adjoined[P] = theAdjoined.asInstanceOf[Adjoined[P]]

  }


  trait WithUnitMonoid[P] extends Monoid[WithUnit[P]] {
    implicit protected[this] def base: Semigroup[P]
    override def empty: WithUnit[P] = WithUnit.unit[P]
    override def combine(x: WithUnit[P], y: WithUnit[P]): WithUnit[P] = x.combine(y)
    override def combineN(a: WithUnit[P], n: Int): WithUnit[P] = a.combineN(n)
  }
  trait WithUnitAdditiveMonoid[P] extends AdditiveMonoid[WithUnit[P]] {
    implicit protected[this] def base: AdditiveSemigroup[P]
    override def zero: WithUnit[P] = WithUnit.unit[P]
    override def plus(x: WithUnit[P], y: WithUnit[P]): WithUnit[P] = x.plus(y)
    override def sumN(a: WithUnit[P], n: Int): WithUnit[P] = a.sumN(n)
  }
  trait WithUnitMultiplicativeMonoid[P] extends MultiplicativeMonoid[WithUnit[P]] {
    implicit protected[this] def base: MultiplicativeSemigroup[P]
    override def one: WithUnit[P] = WithUnit.unit[P]
    override def times(x: WithUnit[P], y: WithUnit[P]): WithUnit[P] = x.times(y)
    override def pow(a: WithUnit[P], n: Int): WithUnit[P] = a.pow(n)
  }


  trait WithUnitCommutativeMonoid[P]
    extends CommutativeMonoid[WithUnit[P]] with WithUnitMonoid[P] {
    implicit protected[this] def base: CommutativeSemigroup[P]
  }
  trait WithUnitAdditiveCommutativeMonoid[P]
    extends AdditiveCommutativeMonoid[WithUnit[P]] with WithUnitAdditiveMonoid[P] {
    implicit protected[this] def base: AdditiveCommutativeSemigroup[P]
  }
  trait WithUnitMultiplicativeCommutativeMonoid[P]
    extends MultiplicativeCommutativeMonoid[WithUnit[P]] with WithUnitMultiplicativeMonoid[P] {
    implicit protected[this] def base: MultiplicativeCommutativeSemigroup[P]
  }
}




