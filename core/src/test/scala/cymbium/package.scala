import cymbium.finitemaps.{MappingN, PermutationN}
import cymbium.generic.FreeNMonoid
import cymbium.modular.Modular
import cymbium.newq2.LinearRepr.LRep
import org.scalacheck.{Arbitrary, Cogen, Gen}
import shapeless.Witness
import shapeless.tag.@@
import spire.algebra.free.FreeMonoid

package object cymbium {

  object arb {
    implicit def arbitraryForLRep[A](implicit aa: Arbitrary[A]): Arbitrary[LRep[A]] = {
      val x = Arbitrary.arbTuple2[A,A].arbitrary.map{ case(a1,a2) => LRep(a1,a2)}
      Arbitrary(x)
    }

    implicit def arbitraryForModular[A <: Int](implicit wa: Witness.Aux[A]): Arbitrary[Modular @@ A] = {
      require(wa.value > 1)
      val r = Modular.ring[A]

      Arbitrary(Gen.choose(0,wa.value - 1).map{i => r.fromInt(i)})
    }

    implicit def arbitraryForFreeMonoid[A](implicit aa: Arbitrary[A]): Arbitrary[FreeMonoid[A]] = {
      val ala = implicitly[Arbitrary[List[A]]]
      implicit val fma = FreeMonoid.FreeMonoidMonoid[A]
      Arbitrary(ala.arbitrary.map { la =>
        fma.combineAll(la.iterator.map{a => FreeMonoid.lift[A](a)})
      })
    }

    implicit def arbitraryForFreeNMonoid[N <: Int](implicit w: Witness.Aux[N]): Arbitrary[FreeNMonoid[N]] = {
      //val x = Gen.
      require(w.value>0)
      val is = Gen.choose(0,w.value-1)
      val ais = Gen.buildableOf[Array[Int],Int](is)

      Arbitrary(ais.map{ ia => FreeNMonoid.fromInts[N](ia) })
    }

    implicit def arbitraryForMappingN[N <: Int](implicit w: Witness.Aux[N]): Arbitrary[MappingN[N]] = {
      require(w.value>0)
      val is = Gen.choose(min=0,max=w.value - 1)
      val mis = Gen.buildableOfN[Array[Int],Int](w.value,is)
      Arbitrary(mis.map { a => MappingN.imageSeq(a) })
    }
    implicit def arbitraryForPermutationN[N <: Int](implicit w: Witness.Aux[N]): Arbitrary[PermutationN[N]] = {
      import spire.syntax.cfor._
      require(w.value>0)
      val gen = if (w.value == 1) {
        Gen.const(PermutationN.permutationNGroup[N].empty)
      } else {

        val choosers = ((w.value - 1) to 1 by -1).map{ c => Gen.choose(min=0,max=c) }
        val steps = Gen.sequence[Array[Int],Int](choosers)
        steps.map { steps =>
          val arr = Array.tabulate(w.value)(Predef.identity)
          cfor(0)(_ < steps.length, _ + 1) {  i =>
              val target = arr.length - 1 - i
              val source = steps(i)
              val tmp = arr(source)
              arr(source) = arr(target)
              arr(target) = tmp
          }
          PermutationN.imageSeq[N](arr)
        }
      }
      Arbitrary(gen)
    }

    /*
    implicit def lrepf[A](implicit aa: Arbitrary[A => A] ): Arbitrary[LRep[A] => LRep[A]] = {
      val x = Arbitrary.arbTuple2[A,A].arbitrary.map{ case(a1,a2) => LRep(a1,a2)}
      Arbitrary(x)
    }
    */
  }

  object cgen {
    implicit def cogenForLRep[A](implicit cg: Cogen[(A,A)]): Cogen[LRep[A]] =
      cg.contramap(s => (s.t0,s.t1))
    implicit def cogenForModular[A <: Int]: Cogen[Modular @@ A] = {
      implicitly[Cogen[Int]].contramap { _.repr }
    }
    implicit def cogenForFreeNMonoid[A <: Int]: Cogen[FreeNMonoid[A]] = {
      implicitly[Cogen[Seq[Int]]].contramap { _.ints }
    }
    implicit def cogenForMappingN[A <: Int]: Cogen[MappingN[A]] = {
      implicitly[Cogen[Seq[Int]]].contramap { _.asIndexedSeq }
    }
    implicit def cogenForPermutationN[A <: Int]: Cogen[PermutationN[A]] = {
      implicitly[Cogen[Seq[Int]]].contramap { _.asIndexedSeq }
    }
  }

}