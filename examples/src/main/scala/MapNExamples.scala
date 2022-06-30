
import cymbium.finitemaps.{MapN, MappingN}
import shapeless.Witness

object MapNExamples {

  private val exMap = collection.concurrent.TrieMap.empty[String, (Vector[String] => Unit)]

  def ex(name: String)(expr: => Unit): Unit = {
    require(!exMap.contains(name))
    exMap.put(name, { _ => expr })
  }

  ex("Basics") {
    import spire.syntax.all._
    type Mapping2 = MappingN[Witness.`2`.T]
    val id = MappingN.image[Witness.`2`.T](0,1)
    val swap = MappingN.image[Witness.`2`.T](1,0)
    val to0 = MappingN.image[Witness.`2`.T](0,0)
    val to1 = MappingN.image[Witness.`2`.T](1,1)

    val els = Vector("id" -> id, "swap" -> swap, "to0" -> to0, "to1" -> to1)

    for(r <- -1 until els.length) {
      for(c <- -1 until els.length) {
        val cell = (r,c) match {
          case (-1,-1) => ""
          case (-1,c) => els(c)._1
          case(r,-1) => els(r)._1
          case (r,c) => (els(r)._2 |+| els(c)._2).toString

        }
        print("%-8s".format(cell))
      }
      println()
    }

  }




  sealed trait PointInfo
  case class PointCycle(cycleMin: Int, stepsFromCycleMin: Int, cycleLength: Int) extends PointInfo
  case class PointChain(target: Int, stepsToTarget: Int) extends  PointInfo


  case class Decomp(points: Vector[PointInfo], fn: Vector[Int]) {

    def cycles: Vector[Vector[Int]] =
      points.zipWithIndex
        .collect{case (x:PointCycle,i) => (x,i)}
        .groupBy(_._1.cycleMin)
        .mapValues(_.sortBy(_._1.stepsFromCycleMin).map{_._2})
        .toArray
        .sortBy{_._1}
        .map{_._2}
        .toVector

    def chains: Vector[Vector[Int]] = {
      val rem = collection.mutable.BitSet.newBuilder.++=(0 until points.length).result()
      rem --= cycles.flatten
      val vb = Vector.newBuilder[Vector[Int]]
      while(!rem.isEmpty) {
        val maxStt = rem.iterator.map{i => points(i).asInstanceOf[PointChain].stepsToTarget}.max
        val start = rem.iterator
          .filter{i => points(i).asInstanceOf[PointChain].stepsToTarget == maxStt}
          .min
        val s = Iterator.iterate(start)(fn).takeWhile(rem).toVector
        vb += (s :+ fn(s.last))
        rem --= s
      }
      vb.result().reverse
    }
  }



  //case class Decomp(cyclesByCycleMin: Map[Int, Vector[PointCycle]])



  def decomp[N <: Int, A <: MapN[N, A]](m: MapN[N,A])(implicit w: Witness.Aux[N]): Decomp = {

    def follow(r: Vector[Option[PointInfo]], history: List[Int], visited: Set[Int], next: Int): Vector[Option[PointInfo]] = {
      if (r(next).isDefined) {
        val (target,stepsToTarget) = r(next).get match {
          case PointCycle(_,_,_) => (next,0)
          case PointChain(t,s) => (t,s)
        }
        history.zipWithIndex.foldLeft(r) {
          case (r,(p,i)) => r.updated(p,Some(PointChain(target=target,stepsToTarget=i+stepsToTarget+1)))
        }
      } else if (visited(next)) {
        val p = history.indexOf(next)
        val cycleMin = history.iterator.take(p+1).min
        val r2 = Iterator.iterate(cycleMin)(m.asFunction).take(p+1).zipWithIndex.foldLeft(r) { case(r,(e,i)) =>
          r.updated(e,Some(PointCycle(cycleMin = cycleMin, stepsFromCycleMin = i, cycleLength = p+1)))
        }
        val r3 = history.iterator.drop(p+1).zipWithIndex.foldLeft(r2) { case(r2,(e,i)) =>
            r.updated(e,Some(PointChain(target=next,stepsToTarget = 1+i)))
        }
        r3
      } else {
        follow(r,next :: history, visited + next, m(next))
      }
    }


    val r = (0 until w.value).foldLeft(Vector.fill[Option[PointInfo]](w.value)(None)) { (v,p) =>
      v(p) match {
        case Some(_) => v
        case None => follow(v,Nil,Set.empty,p)
      }
    }
    Decomp(r.map{_.get},m.asIndexedSeq.toVector)
  }


  ex("Decomp") {
    //Something is wrong - probability of a cycle should increase as size gets larger, but that
    //is not what i see
    val r = new java.util.Random(System.currentTimeMillis())

    val sz = Witness(200)

    val tst = MappingN.imageSeq[sz.T](Array.fill(sz.value)(r.nextInt(sz.value)))
    //val tst = MappingN.image[Witness.`8`.T](1,2,3,6,5,3,7,6)
    val ddtst = decomp(tst)
    for( (d,i) <- ddtst.points.zipWithIndex) {
      println(s" $i : $d")
    }
    println(decomp(tst).fn)
    print(
      ddtst.chains
        .map{_.mkString("|"," ",">")}
        .mkString(" ")
    )
    print(" ")
    println(
      ddtst.cycles.filter(_.length > 1)
        .map{_.mkString("("," ",")")}
        .mkString(" ")
    )
    println(ddtst.cycles.filter(_.length > 1).length)
  }

  def main(as: Array[String]): Unit = {
    if (as.isEmpty) {
      for((k,v) <- exMap) {
        println(s"=== $k ===")
        v(as.toVector)
        println()
      }
    }
  }
}
