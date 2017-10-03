import java.util.concurrent.atomic.AtomicInteger

import Huffman.measureTime
import TreeModels.{Branch, Node}

import scala.collection.mutable

trait StringTool {
  this: Stopwatch with HuffmanTree =>

  //1
  def getCharacterFrequency(string: String): mutable.HashMap[Char, AtomicInteger] = {
    measureTime {
      val map: mutable.HashMap[Char, AtomicInteger] = mutable.HashMap[Char, AtomicInteger]()
      for (c <- string) {
        map.getOrElseUpdate(c, new AtomicInteger(0)).incrementAndGet()
      }
      map
    }
  }

  //https://stackoverflow.com/questions/4965335/how-to-print-binary-tree-diagram re-did one of these in idiomatic scala, optimized it.
  protected def treePrint(n: Node, pref: String = "", isLeft: Boolean = false): Unit = {
    if (n != null) {
      println(pref + (if (isLeft) "|-- " else "\\-- ") + n.toString)
      n match {
        case branch: Branch =>
          if (branch.left != null) treePrint(branch.left, pref + (if (isLeft) "|   " else "    "), isLeft = true)
          if (branch.right != null) treePrint(branch.right, pref + (if (isLeft) "|   " else "    "))
        case _ =>
      }
    }
  }
}
