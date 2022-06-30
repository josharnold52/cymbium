package cymbium.q2

import algebra.Eq
import algebra.ring.CommutativeRing

import scala.annotation.tailrec

object Utilities {


  def simpleEuclidean[A : CommutativeRing : Eq](x: A, y: A)(equoteMod: (A,A) => (A,A)): A = {
    import spire.syntax.ring._
    if (CommutativeRing.isZero(x)) {
      return y
    }
    @tailrec
    def gcd0(y1: A, y2: A): A = {
      println(s"($y1,$y2)")
      if (CommutativeRing.isZero(y2)) y1
      else {
        val (q,s) = equoteMod(y1,y2)
        gcd0(y2, y1 - q * y2)
      }
    }
    gcd0(x,y)
  }


}
