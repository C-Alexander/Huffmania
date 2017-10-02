import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable.{BitSet, HashMap, HashSet}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
def getEncodingMapFromTree(tree: Branch): mutable.HashMap[Char, mutable.BitSet] = {
  time.measureTime {
    val map: mutable.HashMap[Char, mutable.BitSet] = mutable.HashMap[Char, mutable.BitSet]()

    def addBitcodes(tree: Node,
                    location: mutable.BitSet = mutable.BitSet(0),
                    locationIndex: Int = 1): Unit = {
      tree match {
        case (leaf: Leaf) =>
          println(s"adding leaf: ${leaf.char} for location: $location")
          map.put(leaf.char, location)
        case (branch: Branch) =>
          addBitcodes(branch.left, mutable.BitSet() ++ location, locationIndex + 1)
          addBitcodes(branch.right, mutable.BitSet(locationIndex) ++ location, locationIndex + 1)
      }
    }

    addBitcodes(tree)
    println(map.size)
    map
  }
}

//  //5
//  def encodeDataWithMap(data: String, map: mutable.HashMap[Char, mutable.BitSet]): mutable.BitSet = {
//    val result = ListBuffer[Boolean]()
//    val newMap: mutable.HashMap[Char, ListBuffer[Boolean]] = mutable.HashMap[Char, ListBuffer[Boolean]]()
//    for (i <- map) {
//      var bools = i._2
//    }//.map(m => (m._1, m._2.foldLeft(ListBuffer[Boolean]())(_ + _ )))
//    time.measureTime {
//      val b: mutable.BitSet = new mutable.BitSet()
//      var index: Int = 0
//      var currentBitSet: mutable.BitSet = mutable.BitSet()
//      val total: ListBuffer[mutable.BitSet] = ListBuffer[mutable.BitSet]()
//      var str:mutable.StringBuilder = new StringBuilder()
//
//      for (c <- data) {
//      //  currentBitSet = map(c)
//       // currentBitSet.map(_ + index)
//      //  b ++ currentBitSet
//        result += newMap(c)
//      }
//      b.clear()
//      b
//    }
//  }


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
    override def toString: String = s"$frequency:$char"
  }
  case class Branch(left: Node, right: Node, charSet: HashSet[Char], frequency: Int) extends Node {
   // override def toString: String = s"| $frequency:${charSet.mkString("[", ",", "]")} |\n\t/\t\\\n/\t\t\\\n${left.toString}\t\t${right.toString}"
   override def toString: String = s"$frequency"
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
