package cymbium.newq2

import algebra.ring.AdditiveGroup
import cats.kernel.laws.discipline.{EqTests, GroupTests}
import cymbium.generic.{FreeNMonoid, HomomorphismLaws}
import cymbium.newq2.LinearRepr.{LRep, TupleLinearRepr}
import org.scalacheck.{Prop, Properties}
import shapeless.{Witness, the}
import spire.laws.GroupLaws

class LinearReprSpecifications extends Properties("LinearRepr") {

  import cymbium.arb._
  import cymbium.cgen._

  property("TupleLinearRepr") = {
    import cats.syntax.eq._
    import cats.instances.short._

    val ev = new LinearRepr.TupleLinearRepr[Short]
    Prop.forAll { (a1: Short, a2: Short) =>
      val s = ev.of(a1, a2)
      ev.t0(s) === a1 && ev.t1(s) === a2 && s._1 === a1 && s._2 === a2
    }
  }
  property("LRep") = {
    import cats.syntax.eq._
    import cats.instances.short._

    val ev = implicitly[LinearRepr[LRep[Short],Short]]
    //val ev = LRep.linearReprForLRep[Short]
    Prop.forAll { (a1: Short, a2: Short) =>
      val s = ev.of(a1, a2)
      ev.t0(s) === a1 && ev.t1(s) === a2 && s.t0 == a1 && s.t1 == a2
    }
  }


  {

    import algebra.instances.short._
    include(EqTests[LRep[Short]].eqv.all,"LRep[Short]::")
    include(GroupLaws[LRep[Short]].additiveSemigroup.all, prefix="LRep[Short]::")
    include(GroupLaws[LRep[Short]].additiveMonoid.all, prefix="LRep[Short]::")
    include(GroupLaws[LRep[Short]].additiveGroup.all, prefix="LRep[Short]::")
    include(GroupLaws[LRep[Short]].additiveCMonoid.all, prefix="LRep[Short]::")
    include(GroupLaws[LRep[Short]].additiveAbGroup.all, prefix="LRep[Short]::")

    def inc(s:Short) = LinearRepr.lift[Short,LRep[Short]](s)

    val g1 = implicitly[AdditiveGroup[Short]].additive
    val g2 = implicitly[AdditiveGroup[LRep[Short]]].additive

    include(HomomorphismLaws[Short,LRep[Short]](inc).group(g1,g2).all,prefix="LRep[Short]::lift::")
    include(HomomorphismLaws[Short,LRep[Short]](inc).monomorphism.all,prefix="LRep[Short]::lift::")
  }

  {
    import FreeNMonoid.additiveMonoidIntances._
    include(EqTests[LRep[FreeNMonoid[Witness.`3`.T]]].eqv.all, "LRep[FreeNMonoid[3]]::")
    include(GroupLaws[LRep[FreeNMonoid[Witness.`3`.T]]].additiveSemigroup.all, prefix = "LRep[FreeNMonoid[3]]::")
    include(GroupLaws[LRep[FreeNMonoid[Witness.`3`.T]]].additiveMonoid.all, prefix = "LRep[FreeNMonoid[3]]::")
  }
}

object LinearReprSpecifications {

}