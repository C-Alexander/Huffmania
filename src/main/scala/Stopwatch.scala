/**
  * Awesome stopwatch class. Simply include the trait and put your code inside a measureTime block.
  *@example
  * {{{
  *def method(parameter: Int, otherParameter: String): Int = {
  *  measureTime {
  *          doAlsoStuff()
  *          doMore(parameter, otherparameter)
  *         }
  *
  *  measureTime ({
  *          doStuff()
  *          return stuff
  *         }, "Doing stuff") //by giving text as a parameter, you can clarify your printlines!
  *}
  * }}}
  *
  */
trait Stopwatch {

  /**
    *
    * @param code the code to measure the execution time for
    * @param name the name of the action being executed
    * @tparam A the return type
    * @return Just your usual old return. Seamless. :)
    */
  protected def measureTime[A](code: => A, name: String = ""): A = {
    val startTime = System.currentTimeMillis()
    val result = code
    if (name.isEmpty) println("Time passed: " + (System.currentTimeMillis() - startTime))
    else println(s"$name took: " + (System.currentTimeMillis() - startTime))
    result
  }
}
