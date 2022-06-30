package cymbium

import algebra.{CommutativeGroup, CommutativeMonoid, CommutativeSemigroup, Eq}
import algebra.ring._
import cats.{Group, Semigroup}
import spire.algebra.{AdditiveSemigroup, Monoid}
import scala.{specialized => sp}

object convert {
  def from[A](a: A)(implicit cf: Converter[A]): cf.Ops = cf.convertFrom(a)

  trait ConvertFrom[A] {
    val from: A
  }
  class FromGroupOps[A](val from: Group[A]) extends ConvertFrom[Group[A]] { ops =>
    def toAdditiveGroup: AdditiveGroup[A] = new AdditiveGroupFromGroup[A] { protected[this] val from = ops.from }
    def toMultiplicativeGroup: MultiplicativeGroup[A] = new MultiplicativeGroupFromGroup[A]{ protected[this] val from = ops.from }
  }
  class FromCommutativeGroupOps[A](val from: CommutativeGroup[A]) extends ConvertFrom[CommutativeGroup[A]] { ops =>
    def toAdditiveCommutativeGroup: AdditiveCommutativeGroup[A] = new AdditiveCommutativeGroupFromCommutativeGroup[A]{ protected[this] val from = ops.from }
    def toMultiplicativeCommutativeGroup: MultiplicativeCommutativeGroup[A] = new MultiplicativeCommutativeGroupFromCommutativeGroup[A]{ protected[this] val from = ops.from }
  }
  class FromMonoidOps[A](val from: Monoid[A]) extends ConvertFrom[Monoid[A]] { ops =>
    def toAdditiveMonoid: AdditiveMonoid[A] = new AdditiveMonoidFromMonoid[A]{ protected[this] val from = ops.from }
    def toMultiplicativeMonoid: MultiplicativeMonoid[A] = new MultiplicativeMonoidFromMonoid[A]{ protected[this] val from = ops.from }
  }
  class FromCommutativeMonoidOps[A](val from: CommutativeMonoid[A]) extends ConvertFrom[CommutativeMonoid[A]] { ops =>
    def toAdditiveCommutativeMonoid: AdditiveCommutativeMonoid[A] = new AdditiveCommutativeMonoidFromCommutativeMonoid[A]{ protected[this] val from = ops.from }
    def toMultiplicativeCommutativeMonoid: MultiplicativeCommutativeMonoid[A] = new MultiplicativeCommutativeMonoidFromCommutativeMonoid[A]{ protected[this] val from = ops.from }
  }
  class FromSemigroupOps[A](val from: Semigroup[A]) extends ConvertFrom[Semigroup[A]] { ops =>
    def toAdditiveSemigroup: AdditiveSemigroup[A] = new AdditiveSemigroupFromSemigroup[A]{ protected[this] val from = ops.from }
    def toMultiplicativeSemigroup: MultiplicativeSemigroup[A] = new MultiplicativeSemigroupFromSemigroup[A]{ protected[this] val from = ops.from }
  }
  class FromCommutativeSemigroupOps[A](val from: CommutativeSemigroup[A]) extends ConvertFrom[CommutativeSemigroup[A]] { ops =>
    def toAdditiveCommutativeSemigroup: AdditiveCommutativeSemigroup[A] = new AdditiveCommutativeSemigroupFromCommutativeSemigroup[A]{ protected[this] val from = ops.from }
    def toMultiplicativeCommutativeSemigroup: MultiplicativeCommutativeSemigroup[A] = new MultiplicativeCommutativeSemigroupFromCommutativeSemigroup[A]{ protected[this] val from = ops.from }
  }

  trait Converter[A] {
    type Ops
    def convertFrom(a: A): Ops
  }

  class ConcreteConverter[A,O](f: A => O) extends Converter[A] {
    type Ops = O
    def convertFrom(a: A) = f(a)
  }


  object Converter {
    type Aux[A,O] = Converter[A] { type Ops = O }
    def apply[A,O](f: A => O): Converter.Aux[A,O] = new ConcreteConverter(f)
    implicit def groupConverter[A] = Converter { x: Group[A] => new FromMonoidOps(x) }
    implicit def monoidConverter[A] = Converter { x: Monoid[A] => new FromMonoidOps(x) }
    implicit def semigroupConverter[A] = Converter { x: Semigroup[A] => new FromSemigroupOps(x) }
  }



  sealed trait AdditiveSemigroupFromSemigroup[@sp(Int, Long, Float, Double) A]
    extends AdditiveSemigroup[A] {
    protected[this] val from: Semigroup[A]
    override final def plus(x: A, y: A): A = from.combine(x,y)
    override def additive: Semigroup[A] = from
    override final def sumN(a: A, n: Int): A = from.combineN(a,n)
    override final def trySum(as: TraversableOnce[A]): Option[A] = from.combineAllOption(as)
  }
  sealed trait AdditiveMonoidFromMonoid[@sp(Int, Long, Float, Double) A]
    extends AdditiveMonoid[A] with AdditiveSemigroupFromSemigroup[A] {
    protected[this] val from: Monoid[A]
    override final def zero = from.empty
    override def additive: Monoid[A] = from
    override final def isZero(a: A)(implicit ev: Eq[A]): Boolean = from.isEmpty(a)
    override final def sum(as: TraversableOnce[A]): A = from.combineAll(as)
  }
  sealed trait AdditiveGroupFromGroup[@sp(Int, Long, Float, Double) A]
    extends AdditiveGroup[A] with AdditiveMonoidFromMonoid[A] {
    protected[this] val from: Group[A]
    override def additive: Group[A] = from
    override final def minus(x: A, y: A): A = from.remove(x,y)
    override final def negate(x: A): A = from.inverse(x)
  }
  sealed trait AdditiveCommutativeSemigroupFromCommutativeSemigroup[@sp(Int, Long, Float, Double) A]
    extends AdditiveCommutativeSemigroup[A] with AdditiveSemigroupFromSemigroup[A] {
    protected[this] val from: CommutativeSemigroup[A]
    override def additive: CommutativeSemigroup[A] = from
  }
  sealed trait AdditiveCommutativeMonoidFromCommutativeMonoid[@sp(Int, Long, Float, Double) A]
    extends AdditiveCommutativeMonoid[A] with AdditiveMonoidFromMonoid[A] {
    protected[this] val from: CommutativeMonoid[A]
    override def additive: CommutativeMonoid[A] = from
  }
  sealed trait AdditiveCommutativeGroupFromCommutativeGroup[@sp(Int, Long, Float, Double) A]
    extends AdditiveCommutativeGroup[A] with AdditiveGroupFromGroup[A] {
    protected[this] val from: CommutativeGroup[A]
    override def additive: CommutativeGroup[A] = from
  }



  sealed trait MultiplicativeSemigroupFromSemigroup[@sp(Int, Long, Float, Double) A]
    extends MultiplicativeSemigroup[A] {
    protected[this] val from: Semigroup[A]
    override final def times(x: A, y: A): A = from.combine(x,y)
    override def multiplicative: Semigroup[A] = from
    override final def pow(a: A, n: Int): A = from.combineN(a,n)
    override final def tryProduct(as: TraversableOnce[A]): Option[A] = from.combineAllOption(as)
  }
  sealed trait MultiplicativeMonoidFromMonoid[@sp(Int, Long, Float, Double) A]
    extends MultiplicativeMonoid[A] with MultiplicativeSemigroupFromSemigroup[A] {
    protected[this] val from: Monoid[A]
    override final def one = from.empty
    override def multiplicative: Monoid[A] = from
    override final def isOne(a: A)(implicit ev: Eq[A]): Boolean = from.isEmpty(a)
    override final def product(as: TraversableOnce[A]): A = from.combineAll(as)
  }
  sealed trait MultiplicativeGroupFromGroup[@sp(Int, Long, Float, Double) A]
    extends MultiplicativeGroup[A] with MultiplicativeMonoidFromMonoid[A] {
    protected[this] val from: Group[A]
    override def multiplicative: Group[A] = from
    override final def div(x: A, y: A): A = from.remove(x,y)
    override final def reciprocal(x: A): A = from.inverse(x)
  }
  sealed trait MultiplicativeCommutativeSemigroupFromCommutativeSemigroup[@sp(Int, Long, Float, Double) A]
    extends MultiplicativeCommutativeSemigroup[A] with MultiplicativeSemigroupFromSemigroup[A] {
    protected[this] val from: CommutativeSemigroup[A]
    override def multiplicative: CommutativeSemigroup[A] = from
  }
  sealed trait MultiplicativeCommutativeMonoidFromCommutativeMonoid[@sp(Int, Long, Float, Double) A]
    extends MultiplicativeCommutativeMonoid[A] with MultiplicativeMonoidFromMonoid[A] {
    protected[this] val from: CommutativeMonoid[A]
    override def multiplicative: CommutativeMonoid[A] = from
  }
  sealed trait MultiplicativeCommutativeGroupFromCommutativeGroup[@sp(Int, Long, Float, Double) A]
    extends MultiplicativeCommutativeGroup[A] with MultiplicativeGroupFromGroup[A] {
    protected[this] val from: CommutativeGroup[A]
    override def multiplicative: CommutativeGroup[A] = from
  }

}

