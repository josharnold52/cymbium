package cymbium.scratchpad
import scala.language.experimental.macros
import algebra.ring.CommutativeRing
import machinist.DefaultOps

object Mulling {
  import spire.syntax.ring._

  class Byteish(val repr: Byte) extends AnyVal {
    def hex = "%02X".format(repr & 0xFF)

    def map(f: Byte => Int) = new Byteish(f(repr).toByte)

    def combine(b2: Byteish)(f: (Byte, Byte) => Int) = new Byteish(f(repr,b2.repr).toByte)

  }

  object Byteish {

    def apply(s:Byte): Byteish = new Byteish(s)

    def values = (0 until 256).map { x => Byteish(x.toByte) }
  }

  trait TruncatedSignedBinary[T] extends CommutativeRing[T] {
    def lowUnsigned(t: T): T
    def lowSigned(t: T): T
    def highUnsigned(t: T): T
    def highSigned(t: T): T

    def highBitSigned(t: T): T

  }

  implicit object ShortTruncatedSignedBinary extends TruncatedSignedBinary[Byteish] {
    override def highUnsigned(t: Byteish): Byteish = t.map { x => ( x & 0xF0) >> 4 }
    override def highSigned(t: Byteish): Byteish = t.map { _ >> 4 }

    override def lowUnsigned(t: Byteish): Byteish = t.map { _ & 0xF }
    override def lowSigned(t: Byteish): Byteish = t.map { x =>
      (x << 4).toByte.toInt >> 4
    }

    override def minus(x: Byteish, y: Byteish) = x.combine(y) { _ - _ }

    override def plus(x: Byteish, y: Byteish)= x.combine(y) { _ + _ }

    override def times(x: Byteish, y: Byteish) = x.combine(y) { _ * _ }

    override def one: Byteish = Byteish(1)

    override def zero: Byteish = Byteish(0)

    override def negate(x: Byteish): Byteish = x.map { - _ }

    override def highBitSigned(t: Byteish): Byteish = t.map{ _ >> 7 }

  }

  implicit class TruncatedSignedBinaryOps[T : TruncatedSignedBinary](t: T) {
    def highUnsigned: T = macro DefaultOps.unop0[T]
    def lowUnsigned: T = macro DefaultOps.unop0[T]
    def lowSigned: T = macro DefaultOps.unop0[T]
    def highSigned: T = macro DefaultOps.unop0[T]
    def highBitSigned: T = macro DefaultOps.unop0[T]
  }

  def predictOverflowSafe(x: Byteish, y:Byteish): Boolean = {
    val xl = x.repr.toLong
    val yl = y.repr.toLong
    val zl = xl * yl
    zl < Byte.MinValue || zl > Byte.MaxValue
  }
  def altPredict(x: Byteish, y: Byteish): Boolean = {
    val dbg = false
    if (dbg) println("x="+x.hex)
    if (dbg) println("x.highsigned="+x.highSigned.hex)
    if (dbg) println("y="+y.hex)
    val p11 = x.lowUnsigned * y.lowUnsigned   //lowest 2
    if (dbg) println("p11="+p11.hex)
    val p21 = x.highSigned * y.lowUnsigned    //low, high
    if (dbg) println("p21="+p21.hex)
    val p12 = x.lowUnsigned * y.highSigned    //low, high
    if (dbg) println("p12="+p12.hex)
    val p22 = x.highSigned * y.highSigned     //high
    if (dbg) println("p22="+p22.hex)

    val pMix = p12 + p21

    val c2 = p11.highUnsigned + p21.lowUnsigned  + p12.lowUnsigned
    if (dbg) println("c2="+c2.hex)
    val c3 = c2.highUnsigned + p22 + p21.highSigned + p12.highSigned
    if (dbg) println("c3="+c3.hex)
    if (dbg) println("c2.lowSigned="+c2.lowSigned.hex)
    if (dbg) println("c2.lowSigned.highBitSigned="+c2.lowSigned.highBitSigned.hex)

    (((c2.repr >> 3) & 1) + c3.repr) != 0
  }

  def predictOverflow(x: Byteish, y:Byteish, dbg: Boolean = false): Boolean = {
    if (dbg) println("x="+x.hex)
    if (dbg) println("x.highsigned="+x.highSigned.hex)
    if (dbg) println("y="+y.hex)
    val p11 = x.lowUnsigned * y.lowUnsigned   //lowest 2
    if (dbg) println("p11="+p11.hex)
    val p21 = x.highSigned * y.lowUnsigned    //low, high
    if (dbg) println("p21="+p21.hex)
    val p12 = x.lowUnsigned * y.highSigned    //low, high
    if (dbg) println("p12="+p12.hex)
    val p22 = x.highSigned * y.highSigned     //high
    if (dbg) println("p22="+p22.hex)

    val pMix = p12 + p21

    val c2 = p11.highUnsigned + p21.lowUnsigned  + p12.lowUnsigned
    if (dbg) println("c2="+c2.hex)
    val c3 = c2.highUnsigned + p22 + p21.highSigned + p12.highSigned
    if (dbg) println("c3="+c3.hex)
    if (dbg) println("c2.lowSigned="+c2.lowSigned.hex)
    if (dbg) println("c2.lowSigned.highBitSigned="+c2.lowSigned.highBitSigned.hex)

    !(c3.repr == c2.lowSigned.highBitSigned.repr)
  }

  def main(as: Array[String]): Unit = {
    val r = Short.MinValue to Short.MaxValue
    var c = 0L
    var ac = 0L
    var sample : Option[(Int,Int)] = None
    for(x <- Byteish.values ; y <- Byteish.values) {
      val pos = predictOverflowSafe(x,y)
      val po = predictOverflow(x,y)
      if (po != pos) {
        c += 1
        if (!sample.isDefined) {
          sample=Some((x.repr,y.repr))
        }
      }
      if (po != altPredict(x,y)) {
        println(s"Alt Mismatch at ${x.hex} * ${y.hex} = ${(((x.repr & 0xFF) * (y.repr & 0xFF)) & 0xFFFF).toHexString} ")
        ac += 1
      }
    }
    println(s"Mispredicted ${c/((Byteish.values.length * Byteish.values.length).toDouble)}")
    println(sample)
    for( (x,y) <- sample) {
      println("%04X %04X".format(x & 0xFFFF,y & 0xFFFF))

      println(predictOverflow(Byteish(x.toByte),Byteish(y.toByte),true))
    }
    println(s"Alt Mispredicted ${ac/((Byteish.values.length * Byteish.values.length).toDouble)}")
  }

}
