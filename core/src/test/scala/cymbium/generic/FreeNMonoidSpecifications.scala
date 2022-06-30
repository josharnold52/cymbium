package cymbium.generic

import cats.kernel.laws.discipline.EqTests
import org.scalacheck.Properties
import org.typelevel.discipline.Predicate
import shapeless.Witness
import spire.laws.{GroupLaws, RingLaws}

class FreeNMonoidSpecifications extends Properties("FreeNMonoid") {

  import cymbium.arb._
  import cymbium.cgen._

  def addMonoidLaws[A <: Int](implicit x: Witness.Aux[A]): Unit = {

    import FreeNMonoid.monoidIntances._
    import FreeNMonoid.additiveMonoidIntances._
    import FreeNMonoid.multiplicativeMonoidInstances._

    implicit val mis = Predicate.const[FreeNMonoid[A]](true)


    include(EqTests[FreeNMonoid[A]].eqv.all,s"FreeNMonoid[${x.value}]::")
    include(GroupLaws[FreeNMonoid[A]].monoid.all, prefix="FreeNMonoid[${x.value}]::")
    include(GroupLaws[FreeNMonoid[A]].additiveMonoid.all, prefix="FreeNMonoid[${x.value}]::")
    include(RingLaws[FreeNMonoid[A]].multiplicativeMonoid.all, prefix="FreeNMonoid[${x.value}]::")

    //include()

    //include(RingLaws[Modular @@ A].cRing.all,s"mod${x.value}.")

  }

  addMonoidLaws(1)
  addMonoidLaws(5)
  addMonoidLaws(255)
  //addModulus[Witness.`5`.T]
  //addModulus[Witness.`8192`.T]
  //addModulus[Witness.`32761`.T]


}
