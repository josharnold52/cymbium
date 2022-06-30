package cymbium.newq2

import cymbium.modular.Modular
import org.scalacheck.Properties
import org.typelevel.discipline.Predicate
import shapeless.Witness
import shapeless.tag.@@
import spire.laws.RingLaws

class ModularSpecifications extends Properties("ModularProperties") {
  import cymbium.arb._

  def addModulus[A <: Int](implicit x: Witness.Aux[A]): Unit = {
    implicit val p = Predicate[Modular @@ A] { x => x.repr != 0 }

    include(RingLaws[Modular @@ A].cRing.all,s"mod${x.value}.")

  }

  addModulus[Witness.`5`.T]
  addModulus[Witness.`8192`.T]
  addModulus[Witness.`32761`.T]

}
