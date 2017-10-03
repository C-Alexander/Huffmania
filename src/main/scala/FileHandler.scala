import java.io._

//noinspection DangerousCatchAll
trait FileHandler {
  this: Stopwatch =>

  //6
  def saveDataToFile(filename: String, bytes: Array[Byte]): Unit = {
    measureTime({
      try {
        val bos = new BufferedOutputStream(new FileOutputStream(filename))
        bos.write(bytes)
        bos.close()
      } catch {
        case all : Throwable => println(all)
      }
    }, "Saving Data to File")
  }

  def saveDataToFile(filename: String, contents: String): Unit = {
    measureTime({
      try {
      val writer = new BufferedWriter(new FileWriter(filename))
      writer.write(contents)
      writer.close()
      } catch {
        case all : Throwable => println(all)
      }
    }, "Saving Text to File")
  }

  //7
  def loadDataFromFile(filename: String): Array[Byte] = {
    measureTime({
      val file = new File(filename)
      val fis = new FileInputStream(file)
      val b = new Array[Byte](file.length.asInstanceOf[Int])
      fis.read(b)
      fis.close()
      b
    }, "Loading Data from File")
  }
}
