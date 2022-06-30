import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter, PrintWriter}

import spire.syntax.cfor.cfor

import scala.annotation.tailrec

object Castles {
  final val numSoldiers = 100
  final val numCastles = 10


  var idCounter = 0L

  val popDir = new File("/Users/arnold/projects/castles")

  def readPop(name: String): collection.IndexedSeq[Array[Int]] = {
    val a = scala.io.Source.fromFile(new File(popDir,name),"UTF-8")
      .getLines()
      .map{s => val r = s.split(",").map{_.toInt}; require(r.length == 10); r}
      .toArray
    a
  }

  lazy val c1 = readPop("c1.csv")
  lazy val c2 = readPop("c2.csv")
  lazy val cAll: IndexedSeq[Array[Int]] = {
    val r = (c1 ++ c2).toArray
    r
  }


  def unsignedSum(a: Array[Byte]): Int = {
    @tailrec def s(partial: Int, i: Int): Int = {
      if (i >= a.length) partial else s(partial + (a(i)&0xFF),i+1)
    }
    s(0,0)
  }
  def sum(a: Array[Int]): Int = {
    @tailrec def s(partial: Int, i: Int): Int = {
      if (i >= a.length) partial else s(partial + a(i),i+1)
    }
    s(0,0)
  }

  //1 if s0 wins, -1 if s1 wins, 0 if tie
  def play(s0: Array[Int], s1: Array[Int]): Int = {
    @tailrec def round(p0: Int, p1: Int, nextCastle: Int): Int = {
      if (nextCastle >= numCastles) {
        if (p0 > p1) 1
        else if (p0 < p1) -1
        else 0
      } else {
        val c0 = s0(nextCastle)
        val c1 = s1(nextCastle)
        if (c0 > c1) round(p0 + nextCastle + 1, p1, nextCastle + 1)
        else if (c0 < c1) round(p0, p1 + nextCastle + 1, nextCastle + 1)
        else round(p0, p1, nextCastle + 1)
      }
    }
    round(0,0,0)
  }

  final class Strategy private (private val bits: Array[Byte]) {
    require(bits.length == numCastles)
    val soldiers: Array[Int] = {
      val f = 1.0 / unsignedSum(bits)
      val is = bits.map{x => math.round((x&0xFF) * f * numSoldiers).toInt }
      val s1 = sum(is)
      if (s1 != numSoldiers) {
        val iord = is.zipWithIndex.sorted.reverse.take(math.abs(s1 - numSoldiers)).map{_._2}
        for(i <- iord) {
          is(i) += math.signum(numSoldiers - s1)
        }
        assert(sum(is) == numSoldiers,sum(is))
        assert(is.min >= 0)

      }
      is
    }

    override def toString: String = bits.iterator.map{x => "%02X".format(x)}.mkString("-")

    def mutate(rate: Double, rnd: scala.util.Random): Strategy = {
      val cpy = bits.clone()
      cfor(0)(_ < numCastles, _ + 1) { ci =>
        cfor(1)(_ != 0x100, _ << 1) { bit =>
          if (rnd.nextDouble() <= rate) {
            cpy(ci) = (cpy(ci) ^ bit).toByte
          }
        }
      }
      new Strategy(cpy)
    }

    def cross(rhs: Strategy, rnd: scala.util.Random): Strategy = {
      val cpy = Array.ofDim[Byte](numCastles)
      cfor(0)(_ < numCastles, _ + 1) { ci =>
        cfor(1)(_ != 0x100, _ << 1) { bit =>
          val src = if (rnd.nextBoolean()) bits else rhs.bits
          cpy(ci) = (cpy(ci) | (src(ci) & bit)).toByte
        }
      }
      new Strategy(cpy)
    }

    val id = {
      val s = idCounter
      idCounter += 1
      s
    }
  }

  object Strategy {
    def random(r: scala.util.Random): Strategy = {
      val bs = Array.ofDim[Byte](numCastles)
      r.nextBytes(bs)
      new Strategy(bs)
    }
  }


  def eval(s: Strategy, pop1: IndexedSeq[Strategy], rnd: scala.util.Random): Double = {
    val trials = 2000
    val rfrac = 0.25
    val cfrac = 0.25
    Iterator.continually {
      val b = rnd.nextDouble()
      val s1 =
        if (b <= rfrac) Strategy.random(rnd).soldiers
        else if (b <= (rfrac + cfrac)) cAll(rnd.nextInt(cAll.length))
        else pop1(rnd.nextInt(pop1.length)).soldiers
      play(s.soldiers,s1).toDouble
    }.take(trials).sum
  }

  def evals(in: IndexedSeq[Strategy], rnd: scala.util.Random): IndexedSeq[(Strategy,Double)] = {
    in.iterator.map{s => (s,eval(s,in,rnd))}.toArray.sortBy(- _._2)
  }

  def generation(in: IndexedSeq[(Strategy,Double)], rnd: scala.util.Random): IndexedSeq[(Strategy,Double)] = {
    //Require input in ranked order!

    val kf = 0.1
    //val zz = in.iterator.map{s => (s,eval(s,rnd))}.toArray.sortBy(- _._2).take(in.length / 2)
    val keep = in.length / 10
    val newBlood = in.length / 5

    //val ranked = in.toArray.sortBy(- _._2)
    val bld = Array.newBuilder[Strategy]
    bld ++= in.iterator.take(keep).map{_._1}
    bld ++= Iterator.continually(Strategy.random(rnd)).take(newBlood)
    for(_ <- 0 until (in.length - keep - newBlood)) {
      val kr = rnd.nextDouble()

      val pp1 = if (kr <= (kf + kf)) keep else in.length
      val pp2 = if (kr > kf && kr <= (kf + kf + kf)) keep else in.length
      val z1 = in(rnd.nextInt(pp1))._1
      val z2 = in(rnd.nextInt(pp2))._1
      bld += z1.cross(z2,rnd).mutate(0.01,rnd)
    }
    val x = evals(bld.result(),rnd)
    assert(x.length == in.length)
    x
  }


  def main(as: Array[String]):Unit = {

    val saveR = new File(popDir,"runs/data/"
      + System.currentTimeMillis().toHexString.reverse.take(6).reverse + ".tsv")

    saveR.getParentFile.mkdirs()

    val pw = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(saveR))),false)



    val  r= new scala.util.Random(new java.util.Random(33L))

    var gen = evals(Array.fill(500) { Strategy.random(r) },r)

    for(i <- 1 to Int.MaxValue) {
      val peek = gen.take(20).zipWithIndex.map { case (x,idx) =>
        val score = x._2
        val soldiers = x._1.soldiers
        val ord = soldiers.zipWithIndex.sorted.reverse.map{_._2}
        val id = x._1.id
        "%s\t%s\t%s\t%s\t%s\t%s".format(
          i, score, ord.mkString(","), soldiers.mkString(","), id, idx
        )
      }
      for(p <- peek.take(10)) {
        println(p)
      }
      println()
      for(p <- peek) {
        pw.println(p)
        pw.flush()
      }
      gen = generation(gen, r)
    }
  }
}
