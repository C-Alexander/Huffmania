import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import TreeModels._

trait HuffmanTree {
  this: Stopwatch with StringTool =>
  //2
  def getSortedNodes(map: mutable.HashMap[Char, AtomicInteger]): mutable.PriorityQueue[Node] = {
    measureTime {
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
        treePrint(branch) //disable this for a performance boost pl0x
        branch
      }
    }

    measureTime {
      createTree(nodes.clone)
    }
  }

  //3
  def getDynamicTree(nodes: mutable.PriorityQueue[Node]): Branch = {
    def createTree(nodes: mutable.PriorityQueue[Node], currentBranch: Branch = null): Branch = {
      if (nodes.size < 1) {
        treePrint(currentBranch)
        return currentBranch
      }
      if (currentBranch == null) return createTree(nodes, Branch(nodes.dequeue(), nodes.dequeue()))
      createTree(nodes, Branch(nodes.dequeue(), currentBranch))
    }

    measureTime {
      createTree(nodes)
    }
  }

  //4
  def getEncodingMapFromTree(tree: Branch): mutable.HashMap[Char, ArrayBuffer[Boolean]] = {
    measureTime {
      val map: mutable.HashMap[Char, ArrayBuffer[Boolean]] = mutable.HashMap[Char, ArrayBuffer[Boolean]]()

      def addBitcodes(tree: Node,
                      location: ArrayBuffer[Boolean] = ArrayBuffer[Boolean]()): Unit = {
        tree match {
          case (leaf: Leaf) =>
            map.put(leaf.char, location)
          case (branch: Branch) =>
            addBitcodes(branch.left, location.clone() += false)
            addBitcodes(branch.right, location.clone() += true)
        }
      }

      addBitcodes(tree)
      map
    }
  }

  //4-7
  def getDecodingMapFromTree(tree: Branch): mutable.HashMap[ArrayBuffer[Boolean], Char] = {
    measureTime {
      val map: mutable.HashMap[ArrayBuffer[Boolean], Char] = mutable.HashMap[ArrayBuffer[Boolean], Char]()

      def addBitcodes(tree: Node,
                      location: ArrayBuffer[Boolean] = ArrayBuffer[Boolean]()): Unit = {
        tree match {
          case (leaf: Leaf) =>
            map.put(location, leaf.char)
          case (branch: Branch) =>
            addBitcodes(branch.left, location.clone() += false)
            addBitcodes(branch.right, location.clone() += true)
        }
      }

      addBitcodes(tree)
      map
    }
  }

  //6
  def saveSortedNodesToFile(filename: String, nodes: mutable.PriorityQueue[Node]): Unit = {
    measureTime({
      val oos = new ObjectOutputStream(new FileOutputStream(filename))
      oos.writeObject(nodes)
      oos.close()
    }, "Saving Sorted Nodes to File")
  }

  //7
  def loadSortedNodesFromFile(filename: String): mutable.PriorityQueue[Node] = {
    measureTime({
      val ois = new ObjectInputStream(new FileInputStream(filename))
      val o = ois.readObject().asInstanceOf[mutable.PriorityQueue[Node]]
      ois.close()
      o
    }, "Loading Sorted Nodes from File")
  }
}