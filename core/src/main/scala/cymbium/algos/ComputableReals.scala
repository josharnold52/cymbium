package cymbium.algos

import java.nio.{ByteBuffer, ByteOrder}

import spire.algebra.Signed
import spire.math.{Rational, Real, SafeLong}
import spire.random.Generator
import spire.random.rng.BurtleRot2

import scala.annotation.tailrec

object ComputableReals {
  import Real.{Exact,Inexact}

  //private def

  def randomRationalStreamOfIncreasingFineness(g: Generator): Stream[Rational] = {
    def loop(b: SafeLong, n: SafeLong, d: SafeLong): Stream[Rational] = {
      val n2 = n + (if(g.nextBoolean()) b else SafeLong.zero)
      val r = Rational(n2, d)
      r #:: loop(b << 1, n2, d << 1)
    }
    loop(SafeLong.one, SafeLong.zero, SafeLong.two)
  }

  def randomRationalStreamOfIncreasingFineness(min: Rational, max: Rational)(g: Generator): Stream[Rational] = {
    val dist = max - min
    randomRationalStreamOfIncreasingFineness(g).map { r => min + dist * r}
  }

  object nonTerminatingComparisons extends Signed[Real] {
    override def signum(a: Real): Int = a match {
      case Exact(r) => r.signum
      case Inexact(f) => {
        @tailrec
        def chk(p: Int): Int = f(p).signum match {
          case x if x == 0 => chk(p+1)
          case x => x
        }
        chk(0)
      }
    }

    override def abs(a: Real): Real = a.abs

    override def compare(x: Real, y: Real): Int = (x-y).signum()
  }


  private class BoundsToCR(_s: Stream[(Rational, Rational)]) extends (Int => SafeLong) {
    @volatile private[this] var bounds: Stream[(Rational,Rational)] = _s


    @tailrec
    final override def apply(p: Int): SafeLong = {
      val (l,h) = bounds.head
      val al = Exact(l)(p)
      val ah = Exact(h)(p)
      if (al == ah) al
      else {
        bounds = bounds.tail
        apply(p)
      }
    }
  }

  def boundsStreamToReal(bounds: Stream[(Rational, Rational)]): Real = Inexact(new BoundsToCR(bounds))


  private def findSignedPoint(startBits: Int, mid: Rational, maxError: Rational, f: Rational => Real): (Rational, Real, Int, Rational, Int) = {
    def loop(b: Int, as: Stream[Rational]): (Rational, Real, Int, Rational, Int) = {
      f(as.head) match {
        case er @ Exact(r) => (as.head, er, r.signum, r, b)
        case ir @ Inexact(p) => {
          val pb = p(b)
          val s = pb.signum
          if (s != 0) (as.head, ir, s, Rational(pb,SafeLong.one << b), b)
          else loop(b + 1, as.tail)
        }
      }
    }
    //Use a fixed generator for reproducible results
    val gen: Generator = BurtleRot2.create(103, 109, 139, 167)
    loop(startBits, mid #:: randomRationalStreamOfIncreasingFineness(mid - maxError, mid + maxError)(gen))
  }

  //def mag()


  def bisect(f: Rational => Real): Real = {
    val l = nonTerminatingComparisons.signum(f(Rational.zero))
    val r = nonTerminatingComparisons.signum(f(Rational.one))
    if (l == 0) Real.zero
    else if (r == 0) Real.one
    else if (l == r) throw new IllegalArgumentException
    else if (l > 0) bisect { r => -f(r) }
    else {
      def loop(low: Rational, high: Rational, bits: Int): Stream[(Rational, Rational)] = {
        val mid = (low + high) / 2
        val err = (high - low) / 8
        //TODO: Some of the ignored values in the following pattern are non-trivial to calculate - maybe make a function that doesn't calculate them
        val (r,_,s,_,b2) = findSignedPoint(bits,mid,err,f)
        //println("B")
        if (s == 0) Stream.continually((r,r))
        else if (s < 0) (r,high) #:: loop(r,high,b2+1)
        else (low,r) #:: loop(low, r, b2+1)
      }
      boundsStreamToReal((Rational.zero,Rational.one) #:: loop(Rational.zero, Rational.one,0))
    }
  }


  def illinois(f: Rational => Real): Real = {
    val l = nonTerminatingComparisons.signum(f(Rational.zero))
    val r = nonTerminatingComparisons.signum(f(Rational.one))
    if (l == 0) Real.zero
    else if (r == 0) Real.one
    else if (l == r) throw new IllegalArgumentException
    else {
      def loop(x0: Rational, y0: Rational, s0: Int, x1: Rational, y1: Rational, s1: Int, bits: Int): Stream[(Rational, Rational)] = {
        val x2m = x0 - y0 * (x1 - x0) / (y1 - y0)
        val em = (x2m - x0).abs.min((x1-x0).abs) / 16

        val (x2,fx2,s2,y2,b2) = findSignedPoint(bits,x2m,em,f)
        //println((x2-x2m))

        if (s2 == 0) Stream.continually((x2,x2))
        else if (s2 != s1) {
          //println("S "+bits)
          (x1, x2) #:: loop(x1,y1,s1,x2,y2,s2,b2+1)
        } else {
          //println("M "+bits)
          (x0, x2) #:: loop(x0,y0/2,s0,x2,y2,s2,b2+1)
        }
      }
      boundsStreamToReal((Rational.zero,Rational.one) #:: loop(Rational.zero,l,l,Rational.one,r,r,1))
    }
  }





}
