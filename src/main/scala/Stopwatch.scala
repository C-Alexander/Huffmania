class Stopwatch {
  def measureTime[R](code: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = code
    println("Time passed: " + (System.currentTimeMillis() - t0))
    result
  }
}

object Stopwatch {
  def apply(): Stopwatch = new Stopwatch
}
