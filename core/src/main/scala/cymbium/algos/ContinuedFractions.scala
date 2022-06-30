package cymbium.algos

import spire.algebra.{Field, IsReal}
import spire.math.{ConvertableFrom, Rational, SafeLong}

import scala.collection.mutable.ArrayBuffer

object ContinuedFractions {
  import spire.syntax.all._

  def takeTo[A](i: Iterator[A])(p: A => Boolean): Iterator[A] = new Iterator[A] {
    private var stop = false

    override def hasNext: Boolean = !stop && i.hasNext

    override def next(): A = {
      if (!hasNext) throw new NoSuchElementException
      val n = i.next()
      stop |= p(n)
      n
    }
  }

  def extendedContinuedFractionsIterator[A : IsReal : Field](a: A): Iterator[(A,A)] = {
    def decomp(a: A) = {
      val f = a.floor()
      (f, a - f)
    }
    takeTo(
      Iterator.iterate(decomp(a)) { a => decomp(a._2.reciprocal()) }
    ) {_._2.isZero}
  }

  def continuedFractionsA[A : IsReal : Field](a: A): collection.immutable.Iterable[A] =
    new collection.immutable.Iterable[A] {
      def iterator = extendedContinuedFractionsIterator(a).map{_._1}
    }

  def continuedFractions[A : IsReal : Field : ConvertableFrom](a: A): collection.immutable.Iterable[SafeLong] =

    new collection.immutable.Iterable[SafeLong] {
      val cf = implicitly[ConvertableFrom[A]]
      def iterator = extendedContinuedFractionsIterator(a).map{a => SafeLong(cf.toBigInt(a._1))}
    }


  def approximations[A : IsReal : Field : ConvertableFrom](a: A): collection.immutable.Iterable[Rational] = {
    new collection.immutable.Iterable[Rational] {
      val base = continuedFractions(a)
      override def iterator: Iterator[Rational] = Iterator.iterate(1){_ + 1}.map { i =>
        val iter = base.iterator.take(i).to[ArrayBuffer].reverseIterator
        val h = iter.next
        iter.foldLeft(Rational(h)){ case (r,n) => r.reciprocal + n}
      }
    }
  }
}
