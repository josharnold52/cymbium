package cymbium.newq2

import algebra.ring.Semiring
import cats.kernel.laws.EqLaws
import cats.kernel.laws.discipline.EqTests
import org.scalacheck.{Arbitrary, Cogen, Properties}
import org.typelevel.discipline.Predicate
import spire.algebra.Eq
import spire.laws.RingLaws

class SqReducibleSpecifications extends Properties("SqReducible") {


  implicit def moxie[A : Arbitrary](implicit ss: SqReducible.SqRedDef[A]): Arbitrary[ss.SqRed] = Arbitrary(
    Arbitrary.arbTuple2[A,A].arbitrary.map{ case(a1,a2) => ss.SqRed(a1,a2)}
  )
  implicit def roxie[A : Cogen](implicit ss: SqReducible.SqRedDef[A]) =
    implicitly[Cogen[(A,A)]].contramap { s: ss.SqRed => (s.t0,s.t1) }

  def testSemiring[A : Arbitrary : Eq : Cogen : Semiring](name: String)(t2t0: A, t2t1: A): Unit = {

    implicit val sqdef = new SqReducible.SqRedDef[A](t2t0, t2t1)

    implicit val p:Predicate[sqdef.SqRed] =  { x:sqdef.SqRed => true}

    //implicit val sssArb = Arbitrary (Arbitrary.arbTuple2[A,A].arbitrary.map{ case(a1,a2) => sqdef.SqRed(a1,a2)})
    //implicit val sssArb = moxie[A](sqdef)
    //implicit val ssscg = implicitly[Cogen[(A,A)]].contramap { s: sqdef.SqRed => (s.t0,s.t1) }


    include(EqTests[sqdef.SqRed].eqv.all, prefix=s"$name($t2t0,$t2t1)::")

    include(RingLaws[sqdef.SqRed].semiring.all)
  }

  {
    import spire.std.int._
    testSemiring[Int]("int")(2, 3)
  }

}
