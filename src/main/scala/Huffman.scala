import java.io._
import java.nio.ByteBuffer
import java.util
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable.{BitSet, HashMap, HashSet}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Huffman {
  val time = Stopwatch()

  //1
  def getCharacterFrequency(string: String): mutable.HashMap[Char, AtomicInteger] = {
    time.measureTime {
      val map: mutable.HashMap[Char, AtomicInteger] = mutable.HashMap[Char, AtomicInteger]()
      for (c <- string) {
        map.getOrElseUpdate(c, new AtomicInteger(0)).incrementAndGet()
      }
      println(s"unique characters: ${map.size}")
      map
    }
  }

  //2
  def getSortedNodes(map: mutable.HashMap[Char, AtomicInteger]): mutable.PriorityQueue[Node] = {
    time.measureTime {
      map.foldLeft(mutable.PriorityQueue[Node]()(Ordering.by[Node, Int](_.frequency).reverse)) { (q, m) => q += Leaf(m._1, m._2.intValue()) }
    }
  }

  //3
  def getTree(nodes: mutable.PriorityQueue[Node]): Branch = {
    def createTree(nodes: mutable.PriorityQueue[Node]): Branch = {
      val branch: Branch = Branch(nodes.dequeue(), nodes.dequeue())
      nodes.enqueue(branch)
      if (nodes.size > 1) createTree(nodes) else {
        println()
        print(branch) //disable this for a performance boost pl0x
        branch
      }
    }

    time.measureTime {
      createTree(nodes)
    }
  }
  //3
  def getDynamicTree(nodes: mutable.PriorityQueue[Node]): Branch = {
    def createTree(nodes: mutable.PriorityQueue[Node], currentBranch: Branch = null): Branch = {
      if (nodes.size < 1) {
        print(currentBranch)
        return currentBranch }
      if (currentBranch == null) return createTree(nodes, Branch(nodes.dequeue(), nodes.dequeue()))
      createTree(nodes, Branch(nodes.dequeue(), currentBranch))
      }
    time.measureTime {
      createTree(nodes)
    }
  }

  //4
def getEncodingMapFromTree(tree: Branch): mutable.HashMap[Char, ArrayBuffer[Boolean]] = {
  time.measureTime {
    val map: mutable.HashMap[Char,  ArrayBuffer[Boolean]] = mutable.HashMap[Char, ArrayBuffer[Boolean]]()
    def addBitcodes(tree: Node,
                    location:  ArrayBuffer[Boolean] =  ArrayBuffer[Boolean]()): Unit = {
      tree match {
        case (leaf: Leaf) =>
          map.put(leaf.char, location)
        case (branch: Branch) =>
          addBitcodes(branch.left, location.clone() += false)
          addBitcodes(branch.right, location.clone() += true)
      }
    }

    addBitcodes(tree)
    println(map.size)
    map
  }
}

  //4-7
  def getDecodingMapFromTree(tree: Branch): mutable.HashMap[ArrayBuffer[Boolean], Char] = {
    time.measureTime {
      val map: mutable.HashMap[ArrayBuffer[Boolean], Char] = mutable.HashMap[ArrayBuffer[Boolean], Char]()
      def addBitcodes(tree: Node,
                      location:  ArrayBuffer[Boolean] =  ArrayBuffer[Boolean]()): Unit = {
        tree match {
          case (leaf: Leaf) =>
            map.put(location, leaf.char)
          case (branch: Branch) =>
            addBitcodes(branch.left, location.clone() += false)
            addBitcodes(branch.right, location.clone() += true)
        }
      }

      addBitcodes(tree)
      println(map.size)
      map
    }
  }

  //5
  def encodeDataWithMap(data: String, map: mutable.HashMap[Char, ArrayBuffer[Boolean]]): Array[Byte] = {
    time.measureTime {
      val jb: java.util.BitSet = new util.BitSet()
      var i:Int = 0
      for (c <- data) {
        for (q <- map(c)) {
          jb.set(i, q)
          i += 1
        }
      }
      println(s"original Bitset is: $jb")
      jb.toByteArray
    }
  }

  //6
  def saveDataToFile(filename: String, bytes: Array[Byte]): Unit = {
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    bos.write(bytes)
    bos.close()
  }

  //6
  def saveSortedNodesToFile(filename: String, nodes: mutable.PriorityQueue[Node]): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(filename))
    oos.writeObject(nodes)
    oos.close()
  }


  //7
  def loadDataFromFile(filename: String): Array[Byte] = {
    val file = new File(filename)
    val fis = new FileInputStream(file)
    val b = new Array[Byte](file.length.asInstanceOf[Int])
    fis.read(b)
    fis.close()
    b
  }

  //7
  def loadSortedNodesFromFile(filename: String): mutable.PriorityQueue[Node] = {
    val ois = new ObjectInputStream(new FileInputStream(filename))
    var o = ois.readObject().asInstanceOf[mutable.PriorityQueue[Node]]
    ois.close()
    o
  }

  //7
  def decodeDataWithMap(data: Array[Byte], map: mutable.HashMap[ArrayBuffer[Boolean], Char]): String = {
    time.measureTime {
      val jb: java.util.BitSet = util.BitSet.valueOf(data)
      var i:Int = 0
      for (b <- jb.) {
        for (q <- map(c)) {
          jb.set(i, q)
          i += 1
        }
      }
      jb.toByteArray
    }
  }

  //https://stackoverflow.com/questions/4965335/how-to-print-binary-tree-diagram re-did one of these in idiomatic scala, optimized it.
  private def print(n: Node, pref: String = "", isLeft: Boolean = false): Unit = {
    if (n != null) {
      println(pref + (if (isLeft) "|-- " else "\\-- ") + n.toString)
      n match {
        case branch: Branch =>
          if (branch.left != null) print(branch.left, pref + (if (isLeft) "|   " else "    "), isLeft = true)
          if (branch.right != null) print(branch.right, pref + (if (isLeft) "|   " else "    "))
        case _ =>
      }
      }
  }


  abstract class Node() {
    def frequency:Int
  }
  case class Leaf(char: Char, frequency: Int) extends Node {
    override def toString: String = s"$frequency:${char.toString.replace("\n", "\\n")}"
  }
  case class Branch(left: Node, right: Node, charSet: HashSet[Char], frequency: Int) extends Node {
   // override def toString: String = s"| $frequency:${charSet.mkString("[", ",", "]")} |\n\t/\t\\\n/\t\t\\\n${left.toString}\t\t${right.toString}"
   override def toString: String = s"$frequency:${charSet.mkString("[", ",", "]").replace("\n", "\\n")}"
  }

  object Branch {
    def apply(left: Node, right: Node): Branch = {
      (left, right) match {
        case(left: Leaf, right: Leaf) => Branch(left.asInstanceOf[Leaf], right.asInstanceOf[Leaf])
        case(left: Leaf, right: Branch) => Branch(left.asInstanceOf[Leaf], right.asInstanceOf[Branch])
        case(left: Branch, right: Leaf) => Branch(left.asInstanceOf[Branch], right.asInstanceOf[Leaf])
        case _ => Branch(left.asInstanceOf[Branch], right.asInstanceOf[Branch])
      }
    }
    def apply(left: Leaf, right: Leaf): Branch = {
      val branch = new Branch(left, right, HashSet[Char](left.char, right.char), left.frequency + right.frequency)
      branch
    }
    def apply(left: Branch, right: Leaf): Branch = {
      val branch = new Branch(left, right, left.charSet + right.char, left.frequency + right.frequency)
      branch
    }
    def apply(left: Leaf, right: Branch): Branch = {
      val branch = new Branch(left, right, right.charSet + left.char, left.frequency + right.frequency)
      branch
    }
    def apply(left: Branch, right: Branch): Branch = {
      val branch = new Branch(left, right, left.charSet ++ right.charSet, left.frequency + right.frequency)
      branch
    }
    }
  }
