package cymbium.modular

import algebra.ring.CommutativeRing
import shapeless.{Witness, tag}
import spire.algebra.Eq

class Modular private (val repr: Int) extends AnyVal {
  override def toString() = repr.toString
}

object Modular {
  import shapeless.tag._

  type WithModulus[A <: Int] = Modular @@ A

  //def ring(a: Int): CommutativeRing[Modular @@ a.type] = modularRing[a.type]

  implicit def ring[A <: Int](implicit w: Witness.Aux[A]): ModularCommutativeRing[A] = new ModularCommutativeRing

  final class ModularCommutativeRing[A <: Int](implicit w: Witness.Aux[A])
    extends CommutativeRing[Modular @@ A]  with Eq[Modular @@ A] {

    private[this] val _modulus = w.value
    require(_modulus > 1)
    require(_modulus <= Short.MaxValue)

    private def modulus: Int = _modulus

    @inline
    private[this] def t(i: Int): Modular @@ A = new Modular(i).asInstanceOf[Modular @@ A]

    override def negate(x: Modular @@ A): Modular @@ A = t(_modulus - x.repr)

    override def zero: Modular @@ A = t(0)

    override def plus(x: Modular @@ A, y: Modular @@ A): Modular @@ A = {
      val r = (x.repr + y.repr)
      t(if (r >= _modulus) r - _modulus else r)
    }

    override def one: Modular @@ A = t(1)

    override def times(x: Modular @@ A, y: Modular @@ A): Modular @@ A =
      t((x.repr * y.repr) % _modulus)

    override def fromInt(n: Int): Modular @@ A = {
      val z = n % _modulus
      t(if (z < 0) z + _modulus else z)
    }

    override def fromBigInt(n: BigInt): Modular @@ A = {
      val z = (n % _modulus).toInt
      t(if (z < 0) z + _modulus else z)
    }


    override def eqv(x: Modular @@ A, y: Modular @@ A): Boolean = x.repr == y.repr

    override def neqv(x: Modular @@ A, y: Modular @@ A): Boolean = x.repr != y.repr

    override def minus(x: Modular @@ A, y: Modular @@ A): Modular @@ A = {
      val r = x.repr - y.repr
      t(if (r < 0) r + _modulus else r)
    }

    override def hashCode(): Int = _modulus

    override def equals(obj: Any): Boolean = obj match {
      case m:ModularCommutativeRing[_] => _modulus == m.modulus
      case _ => false
    }
  }

}

