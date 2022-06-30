import cymbium.finitemaps.MappingN
import shapeless.Witness
import spire.math.Rational

import scala.annotation.tailrec
import scala.collection.IndexedSeqOptimized

object MCycleCount {


  class Maps(val n: Int) extends Traversable[PartialFunction[Int,Int]] {
    override def foreach[U](f: PartialFunction[Int,Int] => U): Unit = {
      val as = Array.ofDim[Int](n)

      @tailrec
      def inc(p: Int): Boolean = {
        if (p >= n) false
        else {
          val r = as(p) + 1
          if (r < n) {
            as(p) = r
            true
          } else {
            as(p) = 0
            inc(p+1)
          }
        }
      }

      val acol = new PartialFunction[Int,Int] {
        override def isDefinedAt(x: Int): Boolean = x >= 0 && x < n

        override def apply(v1: Int): Int = as(v1)
      }

      do {
        f(acol)
      } while(inc(0))

    }
  }


  def asSeq(n: Int, f: Int => Int): collection.IndexedSeq[Int] = new collection.IndexedSeq[Int]
    with IndexedSeqOptimized[Int,collection.IndexedSeq[Int]] {
    override def length: Int = n

    override def apply(idx: Int): Int = f(idx)
  }

  def check(n: Int, f: Int => Int): Boolean = {
    var marks = 0
    var found = false
    import spire.syntax.cfor._
    cfor(0)(_ < n, _ + 1) { i =>
      val ibit = 1 << i
      if ((marks & ibit) == 0) {
        var touched = ibit
        var cur = i
        do {} while({
          val nx = f(cur)
          val nxbit = 1 << nx
          if ((marks & nxbit)!=0) false
          else if ((touched & nxbit) != 0) {
            if (nx != cur) {
              found = true
            }
            false
          } else {
            touched |= nxbit
            cur = nx
            true
          }
        })
        marks |= touched
      }
    }
    found
  }

  def main(as: Array[String]): Unit = {

    for(n <- 3 to 7) {
      print(s"$n ")
      var cc = 0
      var tm = 0
      for(s <- new Maps(n)) {
        val nn = n
        val wn = Witness(nn)
        val mm = MappingN.apply[wn.T](s)(wn)
        val dd = MapNExamples.decomp(mm)
        if (check(n,s) != (dd.cycles.filter(_.length > 1).length > 0)) {
          System.err.println(asSeq(n,s))
          System.err.println(mm)
          System.err.println(dd)
          System.err.println(dd.cycles)
          System.err.println(check(n,s))

          sys.error("Ooops! "+ asSeq(n,s))
        }
        //MapNExamples.decomp()
        if (check(n,s)) {
          cc += 1
        }
        tm += 1
      }
      println(cc + " " + tm + " " + (1.0 * cc / tm))
    }
  }
}
