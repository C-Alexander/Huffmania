class Stopwatch {
  def measureTime[R](code: => R, name: String = ""): R = {
    val t0 = System.currentTimeMillis()
    val result = code
    if (name.isEmpty) println("Time passed: " + (System.currentTimeMillis() - t0))
    else println(s"$name took: " + (System.currentTimeMillis() - t0))
    result
  }
}

object Stopwatch {
  def apply(): Stopwatch = new Stopwatch
}
