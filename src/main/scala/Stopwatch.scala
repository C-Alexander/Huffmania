trait Stopwatch {
  protected def measureTime[A](code: => A, name: String = ""): A = {
    val startTime = System.currentTimeMillis()
    val result = code
    if (name.isEmpty) println("Time passed: " + (System.currentTimeMillis() - startTime))
    else println(s"$name took: " + (System.currentTimeMillis() - startTime))
    result
  }
}
