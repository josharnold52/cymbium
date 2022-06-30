package cymbium.scratchpad

import shapeless.Witness

class UpperBound[A <: Int](val w: Witness.Aux[A]) {
  val bound: A = w.value
}


object UpperBound {


  def x[A <: Int, B <: Int](w: Witness.Aux[A])(w2: Witness.Aux[B]) = w.value + w2.value
}