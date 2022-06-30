package cymbium.finitemaps

import shapeless.Witness
import spire.algebra.{Eq, Group}

class PermutationN[N <: Int] private(f: Array[Byte],b: Array[Byte])(implicit val witnessN: Witness.Aux[N])
  extends MapN[N,PermutationN[N]]
  with MapNInverse[N,PermutationN[N]] {
  require(witnessN.value <= 256 && witnessN.value >= 1)
  require(b.length == witnessN.value)
  require(f.length == witnessN.value)

  override def apply(n: Int): Int = f(n) & 0xFF

  override protected def buildFrom(f: Int => Int): PermutationN[N] = PermutationN(f)

  override def inverse: PermutationN[N] = new PermutationN[N](b,f)


}

object PermutationN {
  import spire.syntax.cfor._
  def apply[N <: Int](f: Int => Int)(implicit witnessN: Witness.Aux[N]): PermutationN[N] = {
    require(witnessN.value <= 256 && witnessN.value >= 1)
    val fwd = Array.tabulate[Byte](witnessN.value) { i =>
      val fi = f(i)
      if (fi < 0 || fi >= witnessN.value) throw new IllegalArgumentException
      f(i).toByte
    }
    val bwd = Array.ofDim[Byte](witnessN.value)

    val f0 = fwd(0) & 0xFF
    cfor(1)(_ < fwd.length, _ + 1) { i =>
      val fi = fwd(i) & 0xFF
      if (bwd(fi) != 0 || fi == f0) throw new IllegalArgumentException
      bwd(fi) = i.toByte
    }
    new PermutationN(fwd,bwd)
  }

  def imageSeq[N <: Int](is: collection.Traversable[Int])(implicit witnessN: Witness.Aux[N]): PermutationN[N] = {
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
  def image[N <: Int](is: Int*)(implicit witnessN: Witness.Aux[N]): PermutationN[N] = {
    imageSeq(is)
  }



  private object units extends MapNIdentity.IdentityCache[PermutationN] {
    override protected def generateIdentity[N <: Int](implicit w: Witness.Aux[N]): PermutationN[N] = {
      PermutationN.apply(Predef.identity)
    }
  }

  implicit def permutationNGroup[N <: Int](implicit w: Witness.Aux[N]): Group[PermutationN[N]] =
    new MapNInverse.IdentityMapNGroupLTR[N,PermutationN[N]](units.identityWrapper)

  implicit def permutationNEq[N <: Int]: Eq[PermutationN[N]] = MapN.mapNEq[N,PermutationN[N]]

}