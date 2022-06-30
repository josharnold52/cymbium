import algebra.ring.{AdditiveCommutativeMonoid, CommutativeRing}
import cymbium.generic.{MonoidRing, MonoidRingTermCombiner}
import spire.math.Rational

object MonoidRingExamples extends Examples {

  ex("abc") {
    implicit val im = spire.std.int.IntAlgebra.additive
    import spire.std.int._
    import spire.syntax.all._

    val m = MonoidRing[Int,Rational](0 -> 2, 1 -> 3, 1 -> 3)

    val m1 = MonoidRing[Int,Rational](0 -> 1, 1 -> 1)
    println(m1 * m1 * m1 * m1 * m1)

    //val r = MonoidRing.monoidRingCommutativeRingImpl[Int,Rational]
  }

  ex("test2") {
    import spire.std.any._
    import spire.syntax.all._

    val t = (1,2,3)

    implicit val mon = AdditiveCommutativeMonoid[(Int,Int,Int)].additive

    val r = t + t
    println(r)

    val m = MonoidRing(t -> 3)
    println(m)

    val tc = MonoidRingTermCombiner.tc(m)

    println(tc)

    //val x = MonoidRing.monoidRingCommutativeRingImpl[(Int,Int,Int),Int]
    println(m + m)
    println((m * m + m) * (m * m + m))

  }
}
