import cymbium.algos.{ComputableReals, ContinuedFractions}
import spire.math.Real

object ContinuedFractionsExamples extends Examples  {
  import spire.syntax.all._

  ex("Play") {
    val r =  Real.cos((Real.pi * 2 / Real(360)) * 50) //Real(19710502) / Real(19750221) //1 - Real.exp(-3)
    val tk = 50
    val d = r.toDouble
    val cfs = ContinuedFractions.continuedFractions(r)
    val as = ContinuedFractions.approximations(r)

    println(d)
    for ( (s,a) <- cfs.iterator.zip(as.iterator).take(tk)) {
      val ad = a.toDouble
      val err = (ad - d) / d
      println(s"$s $a $ad $err")
    }


  }



}
