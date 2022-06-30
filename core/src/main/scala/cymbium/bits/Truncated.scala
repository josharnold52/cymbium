package cymbium.bits

import shapeless.Nat
import shapeless.ops.nat
import shapeless.ops.nat.ToInt
import shapeless.syntax.nat._

class TruncatedUnsignedInt[N <: Nat] private(val repr: Int) extends AnyVal {

}

object TruncatedUnsignedInt {
  def apply[N <: Nat](value: Int): TruncatedUnsignedInt[N] =
    new TruncatedUnsignedInt[N](value)
}


object TruncatedExercises {
  def main(as: Array[String]): Unit = {

    val x = TruncatedUnsignedInt[Nat._5](22)
    println(x.repr)
  }
}