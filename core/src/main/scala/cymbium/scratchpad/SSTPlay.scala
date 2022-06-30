package cymbium.scratchpad

trait SSTPlay {
  type Init
  type Step1
  type Step2
  type Final

}

object SSTPlay {

  def runPhases(h: SSTPlay)(f: h.Init => h.Final): Unit  = {

  }
}
