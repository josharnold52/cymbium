package cymbium.finitemaps

import cats.kernel.laws.discipline.EqTests
import org.scalacheck.{Prop, Properties}
import shapeless.Witness
import spire.laws.GroupLaws

class PermutationNSpecifications extends Properties("PermutationN") {
  import cymbium.arb._
  import cymbium.cgen._
  def testN[N <: Int](implicit w: Witness.Aux[N]): Unit = {
    include(GroupLaws[PermutationN[N]].monoid.all,s"Mapping[${w.value}]::")
    include(EqTests[PermutationN[N]].eqv.all,s"Mapping[${w.value}]::")
  }

  testN[Witness.`1`.T]
  testN[Witness.`2`.T]
  testN[Witness.`3`.T]
  testN[Witness.`128`.T]
  testN[Witness.`129`.T]
  testN[Witness.`256`.T]



}
