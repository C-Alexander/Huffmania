import java.io._

//noinspection DangerousCatchAll
/**
  * A useful trait for handling files
  */
trait FileHandler {
  this: Stopwatch =>

  /**
    * Save bytes to a file
    * @param filename to save data to
    * @param bytes to save to said file
    */
  protected def saveDataToFile(filename: String, bytes: Array[Byte]): Unit = {
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

  /**
    * Save a string to a file
    * @param filename to save the string to
    * @param contents string to save to said file
    */
  protected def saveDataToFile(filename: String, contents: String): Unit = {
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

  /**
    * Load a byte array from a file
    * @param filename to load from
    * @return contents of the file as Array[Byte]
    */
  protected def loadDataFromFile(filename: String): Array[Byte] = {
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
