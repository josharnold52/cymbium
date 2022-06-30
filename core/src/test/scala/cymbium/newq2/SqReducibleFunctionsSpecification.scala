package cymbium.newq2

import algebra.ring.MultiplicativeSemigroup
import cats.kernel.Eq
import org.scalacheck.{Arbitrary, Prop, Properties, Test}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Predicate
import spire.algebra.Semiring
import spire.laws.RingLaws

class SqReducibleFunctionsSpecification extends Properties("SqReducibleFunctions") {

  def msg[A](term0: A, term1: A)(implicit sra: Semiring[A], eqa: Eq[A], arb: Arbitrary[A]) = {
    implicit val sqr = new SqReducible.TupleSqReducible[A](term0, term1)

    implicit object Sg extends MultiplicativeSemigroup[(A,A)] {
      override def times(x: (A,A), y: (A,A)): (A,A) = SqReducible.times(x,y)
    }
    implicit val teq = cats.instances.tuple.catsKernelStdEqForTuple2[A,A]

    implicit val sxp: Predicate[(A,A)] = Predicate { _ => true}

    val ps = RingLaws[(A,A)].multiplicativeSemigroup.all

    ps
  }


  {
    import spire.std.int._
    include(msg[Int](2,8),"int(8,2)")
  }



  property("n.morphism") = forAll { (a: Int, b: Int) =>
    //println((a,b))
    (a + b) == (b+a)
  }

}
