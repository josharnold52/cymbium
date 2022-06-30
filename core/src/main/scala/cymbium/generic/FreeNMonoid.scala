package cymbium.generic

import java.util

import cats.kernel.Eq
import cymbium.convert
import shapeless.Witness
import spire.algebra.{AdditiveMonoid, Monoid, MultiplicativeMonoid}
import spire.syntax.cfor._

import scala.collection.{GenTraversableOnce, mutable}

class FreeNMonoid[N <: Int] private ( private val els: Array[Byte], val generators: N) {

  def append(i: Int): FreeNMonoid[N] = {
    require(i >= 0 && i < generators)
    val bs = Array.ofDim[Byte](els.length +1)
    java.lang.System.arraycopy(els,0,bs,0,els.length)
    bs(els.length) = i.toByte
    new FreeNMonoid[N](bs,generators)
  }

  def concat(m2: FreeNMonoid[N]): FreeNMonoid[N] =
    if (m2.els.length == 0) this
    else if (els.length == 0) m2
    else {
      val l = els.length + m2.els.length
      if (l < 0) sys.error("Overflow")
      val bs = Array.ofDim[Byte](els.length + m2.els.length)
      java.lang.System.arraycopy(els,0,bs,0,els.length)
      java.lang.System.arraycopy(m2.els,0,bs,els.length,m2.els.length)

      new FreeNMonoid[N](bs,generators)
    }

  def repn(n: Int): FreeNMonoid[N] =
    if (n < 0) sys.error("Invalid n")
    else if (els.length == 0) this
    else if (n == 0) FreeNMonoid.toUnit(this)
    else if (n == 1) this
    else {
      val ll = els.length.toLong * n
      val l = ll.toInt
      if (l != ll) sys.error("Overflow")
      val bs = Array.ofDim[Byte](l)
      cfor(0)(_ < l, _ + els.length) { i =>
        System.arraycopy(els,0,bs,i,els.length)
      }
      new FreeNMonoid[N](bs,generators)
    }

  def repn(n: BigInt): FreeNMonoid[N] =
    if (n < 0) sys.error("Invalid n")
    else if (els.length == 0) this
    else if (n.isValidInt) repn(n.toInt)
    else sys.error("Overflow")

  override def equals(obj: Any): Boolean = obj match {
    case m: FreeNMonoid[_] =>
      generators == m.generators && java.util.Arrays.equals(els,m.els)
    case _ => false
  }

  override def hashCode(): Int = {
    31 * java.util.Arrays.hashCode(els) + generators
  }

  override def toString: String = {
    if (generators < 17) els.iterator.map{i => "%01X".format(i)}.mkString
    else els.iterator.map{i => "[%02X]".format(i)}.mkString
  }

  def ints: collection.immutable.IndexedSeq[Int] = new collection.immutable.IndexedSeq[Int]{
    override def length: Int = els.length

    override def apply(idx: Int): Int = els(idx) & 0xFF
  }
}


object FreeNMonoid {

  private[this] val units: Array[FreeNMonoid[Int]] = Array.tabulate(255)( n =>
    new FreeNMonoid[Int](Array.emptyByteArray,n+1)
  )

  def toUnit[N <: Int](f: FreeNMonoid[N]): FreeNMonoid[N] = {
    val r = units(f.generators - 1)
    assert(r.generators == f.generators)
    r.asInstanceOf[FreeNMonoid[N]]
  }

  //private val theUnit = new FreeNMonoid[Int](Array.emptyByteArray)

  def unit[N <: Int](implicit v: Witness.Aux[N]): FreeNMonoid[N] = {
    require(v.value >= 1 && v.value <= 256)
    val r = units(v.value - 1)
    assert(r.generators == v.value)
    r.asInstanceOf[FreeNMonoid[N]]
  }

  def fromInts[N <: Int](is: collection.Seq[Int])(implicit w: Witness.Aux[N]): FreeNMonoid[N] = {
    val b = newBuilder[N]
    b.sizeHint(is)
    b.++=(is).result()
  }

  def newBuilder[N <: Int](implicit w: Witness.Aux[N]): mutable.Builder[Int,FreeNMonoid[N]] = {
    require(w.value >= 1 && w.value <= 256)
    new mutable.Builder[Int,FreeNMonoid[N]] {
      val bb = Array.newBuilder[Byte]
      override def +=(elem: Int): this.type = {
        require(elem >= 0 && elem < w.value)
        bb += elem.toByte
        this
      }

      override def clear(): Unit = bb.clear()

      override def result(): FreeNMonoid[N] = {
        new FreeNMonoid[N](bb.result(),w.value)
      }

      override def sizeHint(size: Int): Unit = bb.sizeHint(size)
    }
  }

  class FreeNMonoidMonoid[N <: Int](implicit w: Witness.Aux[N]) extends Monoid[FreeNMonoid[N]] {
    override val empty: FreeNMonoid[N] = unit[N]

    override def combine(x: FreeNMonoid[N], y: FreeNMonoid[N]): FreeNMonoid[N] = x.concat(y)

    override def combineN(a: FreeNMonoid[N], n: Int): FreeNMonoid[N] = a.repn(n)

    override protected def repeatedCombineN(a: FreeNMonoid[N], n: Int): FreeNMonoid[N] = a.repn(n)
  }
  class FreeNMonoidEq[N <: Int](implicit w: Witness.Aux[N]) extends Eq[FreeNMonoid[N]] {
    override def eqv(x: FreeNMonoid[N], y: FreeNMonoid[N]): Boolean = x == y
  }

  implicit def eqForFreeNMonoid[N <: Int](implicit w: Witness.Aux[N]): Eq[FreeNMonoid[N]] = new FreeNMonoidEq[N]


  def monoid[N <: Int](implicit w: Witness.Aux[N]): Monoid[FreeNMonoid[N]] = new FreeNMonoidMonoid[N]
  def additiveMonoid[N <: Int](implicit w: Witness.Aux[N]): AdditiveMonoid[FreeNMonoid[N]] =
    convert.from(monoid[N]).toAdditiveMonoid
  def multiplicativeMonoid[N <: Int](implicit w: Witness.Aux[N]): MultiplicativeMonoid[FreeNMonoid[N]] =
    convert.from(monoid[N]).toMultiplicativeMonoid

  object monoidIntances {
    implicit def freeNMonoidToMonoid[N <: Int](implicit w: Witness.Aux[N]) = FreeNMonoid.monoid
  }
  object additiveMonoidIntances {
    implicit def freeNMonoidToAdditiveMonoid[N <: Int](implicit w: Witness.Aux[N]) = FreeNMonoid.additiveMonoid
  }
  object multiplicativeMonoidInstances {
    implicit def freeNMonoidToMultiplicativeMonoid[N <: Int](implicit w: Witness.Aux[N]) = FreeNMonoid.multiplicativeMonoid
  }
}
