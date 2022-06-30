package cymbium.finitemaps

import shapeless.Witness
import shapeless.Witness.Aux
import spire.algebra.{Eq, Monoid}

class MappingN[N <: Int] private(arr: Array[Byte])(implicit val witnessN: Witness.Aux[N]) extends MapN[N,MappingN[N]] {
  require(witnessN.value <= 256 && witnessN.value >= 1)
  require(arr.length == witnessN.value)
  override def apply(n: Int): Int = arr(n) & 0xFF

  override protected def buildFrom(f: Int => Int): MappingN[N] = MappingN.apply(f)
}

object MappingN {

  def apply[N <: Int](f: Int => Int)(implicit witnessN: Witness.Aux[N]): MappingN[N] = new MappingN[N] (
    Array.tabulate[Byte](witnessN.value) { i =>
      val fi = f(i)
      if (fi < 0 || fi >= witnessN.value) throw new IllegalArgumentException
      f(i).toByte
    }
  )

  def imageSeq[N <: Int](is: collection.Traversable[Int])(implicit witnessN: Witness.Aux[N]): MappingN[N] = {
    val iis = is match {
      case a:collection.IndexedSeq[Int] => a
      case a => {
        val ia: collection.IndexedSeq[Int] = a.toArray
        ia
      }
    }
    require(iis.length == witnessN.value)
    apply(iis)
  }
  def image[N <: Int](is: Int*)(implicit witnessN: Witness.Aux[N]): MappingN[N] = {
    imageSeq(is)
  }


  private object units extends MapNIdentity.IdentityCache[MappingN] {
    override protected def generateIdentity[N <: Int](implicit w: Aux[N]): MappingN[N] = {
      MappingN.apply(Predef.identity)
    }
  }

  implicit def mappingNMonoid[N <: Int](implicit witnessN: Witness.Aux[N]): Monoid[MappingN[N]] =
    new MapNIdentity.IdentityMapNMonoidLTR[N,MappingN[N]](units.identityWrapper)
  implicit def mappingNEq[N <: Int]: Eq[MappingN[N]] = MapN.mapNEq[N,MappingN[N]]

}