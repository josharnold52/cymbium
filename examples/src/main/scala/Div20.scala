import scala.collection.mutable

object Div20 {
  def ct20(b: Int): Int = {
    val b8 = b & 0xFF
    val b8_2 = b8 + b8
    val b8_5 = b8 + b8_2 + b8_2
    b8_5 >> 6
  }


  def main(as: Array[String]): Unit = {
    val mp = mutable.HashMap.empty[Int, Set[Int]]
    for(i <- 0 until 256) {

      val z = ct20(i)
      mp.put(z,mp.getOrElse(z,Set.empty) + i)
    }

    println(s"KeyMin = ${mp.keys.min}")
    println(s"KeyMax = ${mp.keys.max}")
    def show[T](f: Iterable[Int] => T): String = mp.toArray.sortBy(_._1).map{z => f(z._2)}.mkString(",")
    println(s"Mins = ${show(_.min)}")
    println(s"Maxs = ${show(_.max)}")
    println(s"Sizes = ${show(_.size)}")

  }
}
