package cymbium.generic

import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws
import spire.algebra.{Eq, Group, Monoid, Semigroup}

object HomomorphismLaws {
  def apply[A: Arbitrary : Eq, B: Eq](f: A => B) = new HomomorphismLaws[A,B] {
    override val morphism: A => B = f
    override val Equa = implicitly
    override val Equb = implicitly
    override val Arb = implicitly
  }
}


trait HomomorphismLaws[A,B] extends Laws{

  def morphism: A => B
  implicit def Equa: Eq[A]
  implicit def Equb: Eq[B]
  implicit def Arb: Arbitrary[A]

  import cats.syntax.eq._

  def semigroup(implicit asg: Semigroup[A], bsg: Semigroup[B]): RuleSet = new SimpleRuleSet(
    name="semigroup homomorphism",
    props=
      "preserves combine" -> Prop.forAll { (a1: A, a2: A) =>
        morphism(asg.combine(a1,a2)) === bsg.combine(morphism(a1),morphism(a2))
        true
      }
  )
  def monoid(implicit am: Monoid[A], bm: Monoid[B]): RuleSet = new DefaultRuleSet(
    name="monoid homomorphism",
    parent = Some(semigroup),
    props=
      "preserves empty" -> Prop.delay {
        morphism(am.empty) === bm.empty
      }

  )
  //For a lawful group we don't need any additional laws, but lets be explicit
  def group(implicit ag: Group[A], bg: Group[B]): RuleSet = new DefaultRuleSet(
    name="group homomorphism",
    parent=Some(monoid),
    props=
      "preserves inverse" -> Prop.forAll { (a: A) =>
        morphism(ag.inverse(a)) === bg.inverse(morphism(a))
        true
      },
      "preserves remove" -> Prop.forAll { (a1: A, a2: A) =>
        morphism(ag.remove(a1,a2)) === bg.remove(morphism(a1),morphism(a2))
        true
      }
  )

  def monomorphism() = new SimpleRuleSet(
    name = "monomorphism",
    props = "is 1:1" -> Prop.forAll { (a1: A, a2: A) =>
      (a1 === a2) == (morphism(a1) == morphism(a2))
    }
  )

}
