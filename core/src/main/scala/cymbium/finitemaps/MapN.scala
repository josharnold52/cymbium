package cymbium.finitemaps

import java.util.concurrent.ConcurrentHashMap

import shapeless.Witness
import shapeless.Witness.Aux
import spire.algebra.{Eq, Group, Monoid, Semigroup}

import scala.language.higherKinds

trait MapN[N <: Int, Repr <: MapN[N,Repr]] { self: Repr  =>
  import spire.syntax.cfor._

  implicit val witnessN: Witness.Aux[N]

  def domainSize: N = witnessN.value
  def apply(n: Int): Int
  def isDefinedAt(x: Int): Boolean =  x >= 0 && x < witnessN.value

  def asFunction: PartialFunction[Int,Int]= new PartialFunction[Int,Int] {
    def apply(n: Int) = self.apply(n)
    override def isDefinedAt(x: Int): Boolean = x >= 0 && x < witnessN.value
  }

  def asIndexedSeq: IndexedSeq[Int] { def length: N } = new IndexedSeq[Int] {
    override def length: N = domainSize
    override def apply(idx: Int): Int = self(idx)
  }

  protected[this] def buildFrom(f: Int => Int): Repr



  def andThen(nxt: Repr): Repr = buildFrom { i => nxt(self(i)) }

  final override def hashCode(): Int = {
    var sum: Int = 0
    val sz = domainSize
    cfor(0)(_ < sz, _ + 1) { i =>
      sum = sum * 31 + apply(i)
    }
    sum
  }

  final override def equals(obj: Any): Boolean = obj match {
    case m:MapN[_,_] => {
      val sz = domainSize
      m.domainSize == sz && {
        var indx = 0
        var eq = true
        while(eq && indx < domainSize) {
          eq = (self(indx) == m(indx))
          indx += 1
        }
        eq
      }
    }

  }

  override def toString: String = asIndexedSeq.iterator.mkString("("," ",")")
}

object MapN {

  class MapNSemigroupLTR[N <: Int, Repr <: MapN[N,Repr]] extends Semigroup[Repr] {

    override def combine(x: Repr, y: Repr): Repr = x.andThen(y)
  }
  class MapNSemigroupRTL[N <: Int, Repr <: MapN[N,Repr]] extends Semigroup[Repr] {

    override def combine(x: Repr, y: Repr): Repr = y.andThen(x)
  }


  private val theMapNEq = Eq.fromUniversalEquals[MapN[_,_]]

  def mapNEq[N <: Int, Repr <: MapN[N,Repr]]: Eq[Repr] = theMapNEq.asInstanceOf[Eq[Repr]]



}



object MapNIdentity {


  class IdentityMapNMonoidLTR[N <: Int, Repr <: MapN[N,Repr] ]( id: Identity[Repr]) extends Monoid[Repr]{
    override def combine(x: Repr, y: Repr): Repr = x.andThen(y)
    override def empty: Repr = id.identity
  }
  class IdentityMapNMonoidRTL[N <: Int, Repr <: MapN[N,Repr] ]( id: Identity[Repr]) extends Monoid[Repr]{
    override def combine(x: Repr, y: Repr): Repr = y.andThen(x)
    override def empty: Repr = id.identity
  }

  trait Identity[Repr] {
    def identity: Repr
  }

  trait IdentityN[Repr[_ <: Int]] {
    def identity[N <: Int](implicit w: Witness.Aux[N]): Repr[N]
    final def identityWrapper[N <: Int](implicit w: Witness.Aux[N]): Identity[Repr[N]] = {
      val id = identity[N]
      new Identity[Repr[N]] {
        val identity = id
      }
    }
  }

  trait IdentityCache[Repr[_ <: Int]] extends IdentityN[Repr] {
    private[this] val mm = new ConcurrentHashMap[Int,Any]()
    protected[this] def generateIdentity[N <: Int](implicit w: Witness.Aux[N]): Repr[N]

    override final def identity[N <: Int](implicit w: Aux[N]): Repr[N] = {
      val x = mm.get(w.value)
      if (x != null) x.asInstanceOf[Repr[N]]
      else {
        val y = generateIdentity[N]
        require(y != null)
        mm.putIfAbsent(w.value,y)
        mm.get(w.value).asInstanceOf[Repr[N]]
      }
    }
  }

}

trait MapNInverse[N <: Int, Repr <: MapN[N,Repr] with MapNInverse[N,Repr]] { self: Repr =>
  def inverse: Repr
}

object MapNInverse {
  class IdentityMapNGroupLTR[N <: Int, Repr <: MapN[N,Repr] with MapNInverse[N,Repr] ]( id: MapNIdentity.Identity[Repr]) extends Group[Repr]{
    override def combine(x: Repr, y: Repr): Repr = x.andThen(y)
    override def empty: Repr = id.identity
    override def inverse(a: Repr): Repr = a.inverse
  }
  class IdentityMapNGroupRTL[N <: Int, Repr <: MapN[N,Repr] with MapNInverse[N,Repr] ]( id: MapNIdentity.Identity[Repr]) extends Group[Repr]{
    override def combine(x: Repr, y: Repr): Repr = y.andThen(x)
    override def empty: Repr = id.identity
    override def inverse(a: Repr): Repr = a.inverse
  }



}