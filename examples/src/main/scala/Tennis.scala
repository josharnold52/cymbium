import algebra.ring.Field
import spire.algebra.IsReal

object Tennis {
  import spire.syntax.all._

  def deuceProb[T: Field](pointProb: T): T = {
    val noPoint = Field.one[T] - pointProb
    val p2 = pointProb * pointProb
    val np2 = noPoint * noPoint
    p2 / (p2 + np2)
  }

  def gameWinProb[T : Field](pointProb: T, winTotal: Int): T = {
    val noPointProb = Field.one[T] - pointProb
    val deuceP = deuceProb(pointProb)

    def run(wins: Int, losses: Int, prob: T): T = {
      if (wins >= winTotal && wins == losses) prob * deuceP
      else if (wins >= winTotal && (wins - losses) >= 2) prob
      else if (losses >= winTotal && (losses - wins) >= 2) Field.zero[T]
      else {
        run(wins+1,losses,prob * pointProb) + run(wins,losses+1,prob * noPointProb)
      }
    }

    run(0,0,Field.one[T])
  }

  def setProb[T: Field](pointProb: T, winsNeeded: Int): T = {
    val g4p = gameWinProb(pointProb, 4)
    val ng4p = Field.one[T] - g4p
    val g7p = gameWinProb(pointProb, 7)

    def run(wins: Int, losses: Int, prob: T): T = {
      if (wins >= winsNeeded && (wins - losses) >= 2) prob
      else if (losses >= winsNeeded && (losses - wins) >= 2) Field.zero[T]
      else if (wins == winsNeeded && losses == winsNeeded) prob * g7p
      else {
        run(wins+1,losses, prob * g4p) + run(wins,losses+1,prob*ng4p)
      }
    }
    run(0,0,Field.one[T])
  }

  def matchProb[T: Field](pointProb: T, winsNeeded: Int): T = {
    val sp = setProb(pointProb, 6)
    val nsp = Field.one[T] - sp
    def run(wins: Int, losses: Int, prob: T): T = {
      if (wins >= winsNeeded) { assert(wins > losses); prob}
      else if (losses >= winsNeeded) { assert(losses>wins); Field.zero[T] }
      else {
        run(wins+1,losses, prob * sp) + run(wins, losses+1, prob * nsp)
      }
    }

    run(0,0,Field.one[T])
  }

  def main(as: Array[String]): Unit = {
    import spire.math.Rational.RationalAlgebra
    import spire.std.double._


    val pointProb = r"55/100"
    //val pointProb = 0.55

    def disp[T : IsReal](label: String, t: T): Unit =
      println(s"$label = ${IsReal[T].toDouble(t)} ($t)")

    disp("PointProb",pointProb)
    disp("DeuceProb",deuceProb(pointProb))
    for(i <- 0 to 7) {
      disp(s"GameTo$i", gameWinProb(pointProb,i))
    }
    disp(s"SP", setProb(pointProb, 6))
    disp(s"M2P", matchProb(pointProb,2))
    disp(s"M3P", matchProb(pointProb,3))
  }
}
