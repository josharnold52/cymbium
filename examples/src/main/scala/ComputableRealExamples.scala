import cymbium.algos.ComputableReals
import spire.math.{Rational, Real, SafeLong}
import spire.math.Real.Exact
import spire.random.Generator

object ComputableRealExamples extends Examples  {

  def nroot(x: Real, k: Int): Real = {
    import spire.syntax.all._
    if (k >= 0) {
      Real(p => {
        val r1 = x((p+1) * k).nroot(k)
        Rational(r1,SafeLong.two).round.toSafeLong
      })
      //Real.roundUp(Rational(value, SafeLong(2).pow(bits - p)))
    } else {
      Real(p => x.reciprocal.nroot(math.abs(k))(p))
      ???
    }
  }

  ex("bisect-test") {
    val x = ComputableReals.bisect { r =>
      2 * r *r  - 1
    }
    val y = Real(Rational(1,2)).nroot(2)
    val yfix = nroot(Real(Rational(1,2)),2)
    val xi = ComputableReals.illinois { r =>
      2 * r *r  - 1
    }

    //WHAT??? Error?

    /*
    val p = 4000
    val (ty,ry) = time(y(p))
    val (txi,rxi) = time(xi(p))
    val (tx,rx) = time(x(p))

    println("x : "+x +" ("+tx+") " + (ry - rx))
    println("xi: "+xi +" ("+txi+") " + (ry - rxi))
    println("y : "+y + " ("+ty+")")

     */
    //val y = x2 - Real.cos(x2)

    //println(x(p+1) - y(p+1))

    val badp = Iterator.from(0,1).find { i => x(i) != yfix(i) }
    println(badp)
    for(bp <- badp) {
      println(x(bp))
      println(xi(bp))
      println(y(bp))
    }
    println(x)
    println(y)

    for(i <- Iterator.from(0,1).take(0)) {
      if (x(i) != y(i)) {
        println((i,x(i) - y(i)));
      }
    }

    //println(ComputableReals.nonTerminatingComparisons.signum(x - Real.cos(x).pow(2)))
  }


  noex(name = "randomRationalStreamOfIncreasingFineness") {
    val s = ComputableReals.randomRationalStreamOfIncreasingFineness(Generator.rng)

    for (x <- s.take(1000)) {
      println(x.toDouble)
    }
  }
}
