package cymbium.generic

import algebra.CommutativeSemigroup
import algebra.ring.{Rig => _, Ring => _, Rng => _, Semiring => _, _}
import cats.kernel.{CommutativeMonoid, Hash}
import spire.algebra._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

class MonoidRing[M,A] private (val terms: collection.immutable.Iterable[(M,A)]) {
  override def toString: String = terms.iterator.map{case(m,a) => s"$a<$m>"}.mkString(" + ")
}

sealed trait MonoidRingTerms[M,A] {
  def terms: collection.immutable.Iterable[(M,A)]
}


sealed trait MonoidRingTermCombiner[M,A] {
  private[generic] def eqm: Eq[M]
  private[generic] def eqa: Eq[A]
  private[generic] def combine(x: collection.Iterator[(M,A)]): collection.Iterable[(M,A)]
  private[generic] def eq(x: collection.Iterable[(M,A)], y: collection.Iterable[(M,A)]): Boolean
}

object MonoidRingOptions {
  implicit object allowEqOnlyMonoids
}

trait MonoidRingTermCombinerLow2 {
  import spire.syntax.all._

  implicit def implicitEqOnlyCombiner[A : Eq : AdditiveCommutativeMonoid, M : Eq](
    implicit enable:MonoidRingOptions.allowEqOnlyMonoids.type
  ): MonoidRingTermCombiner[M,A] = eqOnlyCombiner


  def eqOnlyCombiner[A : Eq : AdditiveCommutativeMonoid, M : Eq]: MonoidRingTermCombiner[M,A] = new MonoidRingTermCombiner[M,A] {
    def eqm = implicitly[Eq[M]]
    def eqa = implicitly[Eq[A]]

    override def combine(x: Iterator[(M, A)]): Iterable[(M, A)] = {
      val src = x.toArray
      val dest = ArrayBuffer.empty[(M,A)]
      for( t <- src) {
        val i = dest.indexWhere { _._1 == t._1 }
        if (i >= 0) {
          dest(i) = (t._1, dest(i)._2 + t._2)
        } else {
          dest += t
        }
      }
      dest
    }

    override def eq(x: Iterable[(M, A)], y: Iterable[(M, A)]): Boolean = {
      x.size == y.size && {
        x.forall { case (kx,vx) =>
          y.find{ _._1 === kx} match {
            case Some((_,vy)) => vx === vy
            case None => false
          }
        }
      }
    }

    override def toString: String = "eqOnly"
  }

}

trait MonoidRingTermCombinerLow extends MonoidRingTermCombinerLow2 {
  implicit def hashCombiner[A : Eq : AdditiveCommutativeMonoid, M : Hash]: MonoidRingTermCombiner[M,A] = new MonoidRingTermCombiner[M,A] {
    import spire.syntax.all._
    def eqm = implicitly[Hash[M]]
    def eqa = implicitly[Eq[A]]
    private class Key(val k: M) {
      override def hashCode(): Int = implicitly[Hash[M]].hash(k)

      override def equals(obj: Any): Boolean = obj match {
        case kk: Key => k === kk.k
        case _ => false
      }

      override def toString: String = k.toString
    }

    override def combine(x: Iterator[(M, A)]): Iterable[(M, A)] = {
      val m = collection.mutable.HashMap.empty[Key,A]
      for((k,v) <- x) {
        val kk = new Key(k)
        m(kk) = m.get(kk) match {
          case Some(v0) => v0 + v
          case None => v
        }
      }
      new Iterable[(M, A)] {
        override def iterator: Iterator[(M, A)] =
          m.iterator.collect { case(k,v) if !implicitly[AdditiveCommutativeMonoid[A]].isZero(v) => (k.k,v) }
      }
    }

    override def eq(x: Iterable[(M, A)], y: Iterable[(M, A)]): Boolean = {
      if (x.size != y.size) false else {
        val m = collection.mutable.HashMap.empty[Key,A]
        //Could maybe do this a bit more efficiently
        m ++= (x.iterator.map{ case(k,v) => (new Key(k),v) })
        y.forall { case(k,v) =>
          m.get(new Key(k)) match {
            case Some(vy) if v === vy => true
            case None => false
          }
        }
      }

    }

    override def toString: String = "hash"
  }
}


object MonoidRingTermCombiner extends MonoidRingTermCombinerLow {
  import spire.syntax.all._

  def apply[M,A](implicit tc: MonoidRingTermCombiner[M,A]) = tc
  def tc[M,A](mr: MonoidRing[M,A])(implicit tc: MonoidRingTermCombiner[M,A]) = tc


  implicit def orderCombiner[A : Eq : AdditiveCommutativeMonoid, M : Order]: MonoidRingTermCombiner[M,A] = new MonoidRingTermCombiner[M,A] {
    def eqm = implicitly[Order[M]]
    def eqa = implicitly[Eq[A]]
    override def combine(x: Iterator[(M, A)]): Iterable[(M, A)] = {
      val arr = x.toArray
      if (arr.isEmpty) arr else {
        {
          implicit val order = Order.by { t: (M, A) => t._1 }
          spire.math.Sorting.quickSort(arr)
        }
        new Iterable[(M,A)] {
          override def iterator: Iterator[(M, A)] =
            nonNormalizedIterator.filter { t =>
              !implicitly[AdditiveCommutativeMonoid[A]].isZero(t._2)
            }
          private def nonNormalizedIterator: Iterator[(M, A)] = new Iterator[(M,A)] {
            val base = arr.iterator
            var work: Option[(M,A)] = Some(base.next())
            override def hasNext: Boolean = work.isDefined

            @tailrec
            private[this] def advance(m: M, a: A): ((M,A),Option[(M,A)]) = {
              if (!base.hasNext) ((m,a),None)
              else base.next() match {
                case (mn,an) if mn === m => advance(m, a + an)
                case r => ((m,a),Some(r))
              }
            }

            override def next(): (M, A) = {
              val (m,a) = work.get
              val (r,nx) = advance(m,a)
              work= nx
              r
            }
          }
        }
      }
    }




    override def eq(x: Iterable[(M, A)], y: Iterable[(M, A)]): Boolean = {
      if (x.size != y.size) false else {
        val xa = x.toArray
        val xb = y.toArray ;
        {
          implicit val order = Order.by { t: (M, A) => t._1 }
          spire.math.Sorting.quickSort(xa)
          spire.math.Sorting.quickSort(xb)
        }
        spire.std.array.ArrayEq(spire.std.tuples.EqProduct2[M,A]).eqv(xa,xb)
      }
    }

    override def toString: String = "order"
  }


}

object MonoidRing {
  import spire.syntax.all._

  def apply[M,A](terms: (M,A)*)(implicit tc: MonoidRingTermCombiner[M,A]): MonoidRing[M,A] = {
    new MonoidRing(MRContaner.newBuilder.++=(tc.combine(terms.iterator)).result())
  }




  private[MonoidRing] type Unused[X] = Any




  private[MonoidRing] trait Base[A,+AA[_],M,+MA[_]] {
    implicit private[MonoidRing] def alga: AA[A]
    implicit private[MonoidRing] def algm: MA[M]
    implicit private[MonoidRing] def oc: MonoidRingTermCombiner[M,A]
  }

  private[MonoidRing] class BaseImpl[A,AA[_],M,MA[_]](
    private[MonoidRing] val alga: AA[A],
    private[MonoidRing] val algm: MA[M],
    private[MonoidRing] val oc: MonoidRingTermCombiner[M,A]
  ) extends Base[A,AA,M,MA]

  trait MonoidRingEq[M,A] extends Eq[MonoidRing[M,A]]
    with Base[A,Unused,M,Unused] {
    override def eqv(x: MonoidRing[M, A], y: MonoidRing[M, A]): Boolean =
      oc.eq(x.terms,y.terms)
  }


  private type MRContainer[X <: (Any,Any)] = Vector[X]
  val MRContaner = Vector


  trait MonoidRingSemiring[M,A] extends Semiring[MonoidRing[M,A]]
    with Base[A,Semiring,M,Semigroup] {
    override final def plus(x: MonoidRing[M, A], y: MonoidRing[M, A]): MonoidRing[M, A] = {
      val r = oc.combine(x.terms.iterator ++ y.terms.iterator)
      new MonoidRing(MRContaner.newBuilder.++=(r).result())
    }

    override final def times(x: MonoidRing[M, A], y: MonoidRing[M, A]): MonoidRing[M, A] = {
      val r = oc.combine(
        for((kx,vx) <- x.terms.iterator; (ky,vy) <- y.terms.iterator) yield {
          val kr = kx |+| ky
          val vr = vx * vy
          (kr,vr)
        }
      )
      new MonoidRing(MRContaner.newBuilder.++=(r).result())
    }


    override final def zero: MonoidRing[M, A] = new MonoidRing(MRContaner.empty)

    override final def isZero(a: MonoidRing[M, A])(implicit ev: Eq[MonoidRing[M, A]]): Boolean = a.terms.isEmpty
  }


  trait MonoidRingCommutativeSemiring[M,A] extends CommutativeSemiring[MonoidRing[M,A]]
    with MonoidRingSemiring[M,A]
    with Base[A,CommutativeSemiring,M,CommutativeSemigroup]

  trait MonoidRingRig[M,A] extends Rig[MonoidRing[M,A]]
    with MonoidRingSemiring[M,A]
    with Base[A,Rig,M,Monoid] {
    override final def one: MonoidRing[M, A] = new MonoidRing(MRContaner(algm.empty -> alga.one))

    override final def isOne(a: MonoidRing[M, A])(implicit ev: Eq[MonoidRing[M, A]]): Boolean = {
      a.terms.size == 1 && {
        val (k,v) = a.terms.head
        alga.isOne(v)(oc.eqa) && algm.isEmpty(k)(oc.eqm)
      }
    }
  }
  trait MonoidRingCommutativeRig[M,A] extends CommutativeRig[MonoidRing[M,A]]
    with MonoidRingRig[M,A]
    with Base[A,CommutativeRig,M,CommutativeMonoid]


  trait MonoidRingRng[M,A] extends Rng[MonoidRing[M,A]]
    with MonoidRingSemiring[M,A]
    with Base[A,Rng,M,Monoid] {
    override final def negate(x: MonoidRing[M, A]): MonoidRing[M, A] = {
      new MonoidRing(
        x.terms.map{case (k,v) => (k,-v)}
      )
    }
  }
  trait MonoidRingCommutativeRng[M,A] extends CommutativeRng[MonoidRing[M,A]]
    with MonoidRingRng[M,A]
    with Base[A,CommutativeRng,M,CommutativeMonoid]


  trait MonoidRingRing[M,A] extends Ring[MonoidRing[M,A]]
    with MonoidRingRig[M,A]
    with MonoidRingRng[M,A]
    with Base[A,Ring,M,Monoid]

  trait MonoidRingCommutativeRing[M,A] extends CommutativeRing[MonoidRing[M,A]]
    with MonoidRingRing[M,A]
    with Base[A,CommutativeRing,M,CommutativeMonoid]



  class MonoidRingCommutativeRingImpl[M,A]()
    (implicit mm: CommutativeMonoid[M], ra: CommutativeRing[A], oc: MonoidRingTermCombiner[M,A])
    extends BaseImpl[A,CommutativeRing,M,CommutativeMonoid](ra,mm,oc) with MonoidRingCommutativeRing[M,A]


  implicit def monoidRingCommutativeRingImpl[M,A]
   (implicit mm: CommutativeMonoid[M], ra: CommutativeRing[A], oc: MonoidRingTermCombiner[M,A]):
      CommutativeRing[MonoidRing[M,A]] = new MonoidRingCommutativeRingImpl[M,A]


}