import scala.collection.concurrent.TrieMap

trait Examples {

  val miniApps = TrieMap.empty[String, (Vector[String] => Int)]

  def main(as: Array[String]): Unit = {
    if (as.isEmpty) {
      for(k <- miniApps.keysIterator.toArray.sorted) {
        println(s"==== $k ====")
        val i = try {
          miniApps(k)(as.toVector)
        } catch {
          case t: Throwable => {
            t.printStackTrace()
            1
          }
        }
        println(s"**** Result: $i ****")
        println()
      }
      System.exit(0)
    }
    ???
  }


  def time[A]( a: => A): (Long, A) = {
    val s = System.nanoTime()
    val ea = a
    val e = System.nanoTime()
    (e - s,ea)
  }

  def ex(name: String)(a: => Unit): Unit = {
    miniApps.put(name, {_ => a ; 0})
  }

  def noex(name: String)(a: => Unit): Unit = ()

}
