package cymbium.newq2

import algebra.ring.{Rig, Semiring}
import cymbium.newq2.LinearRepr.{GenLinearRepr, LRep, TupleLinearRepr}
import cymbium.newq2.LinearReprInstances.AdditiveCommutativeSemigroupForLinearRepr
import spire.algebra.Eq
import scala.language.higherKinds

trait SqReducible[S,A] extends LinearRepr[S,A] {
  val d1sq: S
}
object SqReducible extends SqReducibleFunctions[SqReducible] {
  type GenSqReducible[S[_],A] = SqReducible[S[A],A]

  class TupleSqReducible[A](val d1sq: (A,A)) extends TupleLinearRepr[A] with SqReducible[(A,A),A] {
    def this(d1sqt0: A, d1sqt1: A) = this((d1sqt0,d1sqt1))
  }


  class SqRedDef[A](val d1sqt0: A, d1sqt1: A) { outer =>

    case class SqRed(t0: A, t1: A)
    object SqRed {
      implicit object SqReducibleForSqRed extends SqReducible[SqRed,A] { self =>
        def t0(s: SqRed):A = s.t0
        def t1(s: SqRed):A = s.t1
        def of(t0: A, t1: A): SqRed = SqRed(t0,t1)
        val d1sq = SqRed(d1sqt0,d1sqt1)

      }


      implicit def eqForSqRed(implicit eqa: Eq[A]): Eq[SqRed] = LinearReprInstances.eqForLinearRepr[SqRed,A]

      implicit def semiringForSqRed(implicit sr: Semiring[A]): Semiring[SqRed] =
        LinearReprInstances.semiringForSqReducible[SqRed,A]
      //override protected[this] def linearRepr[A] = sqReducibleForLRep[A]

    }
  }
}


