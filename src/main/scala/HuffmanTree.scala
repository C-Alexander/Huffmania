import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import TreeModels._

trait HuffmanTree {
  this: Stopwatch with StringTool =>
  /**
    * Get the nodes, in a priority queue with the lowest frequency node always retrieved when calling .dequeue()
    * @param map to generate a priorityqueue from
    * @return priority queue, ordered by frequency with the lowest dequeue'd first
    */
  protected def getSortedNodes(map: mutable.HashMap[Char, AtomicInteger]): mutable.PriorityQueue[Node] = {
    measureTime {
      map.foldLeft(mutable.PriorityQueue[Node]()(Ordering.by[Node, Int](_.frequency).reverse)) { (q, m) => q += Leaf(m._1, m._2.intValue()) }
    }
  }

  /**
    * Get the tree from a priority queue, print it while at it
    * @param nodes in a priorityqueue to generate a tree with.
    * @return root branch that the tree stems from
    */
  protected def getTree(nodes: mutable.PriorityQueue[Node]): Branch = {
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

  /**
    * Get a tree that always has leafs on the same side, improves Codec speed
    * @param nodes in a priorityqueue to generate a tree with.
    * @return root branch that the tree stems from
    */
  protected def getDynamicTree(nodes: mutable.PriorityQueue[Node]): Branch = {
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

  /**
    * Get an encoding map for a tree, to quickly look up the corresponding bitcode for a char
    * @param tree to get an encoding map for
    * @return Encoding map with as key a Char and as value an arraybuffer of booleans
    */
  protected def getEncodingMapFromTree(tree: Branch): mutable.HashMap[Char, ArrayBuffer[Boolean]] = {
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

  /**
    * Get a decoding map for a tree, to quickly look up the corresponding char for a bitcode
    * @param tree to get an decoding map for
    * @return Decoding map with as key an arraybuffer of booleans and as value a Char
    */
  protected def getDecodingMapFromTree(tree: Branch): mutable.HashMap[ArrayBuffer[Boolean], Char] = {
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

  /**
    * Save a priorityqueue of nodes to a file
    * @param filename to save the sorted nodes to
    * @param nodes to serialize to the file
    */
  protected def saveSortedNodesToFile(filename: String, nodes: mutable.PriorityQueue[Node]): Unit = {
    measureTime({
      val oos = new ObjectOutputStream(new FileOutputStream(filename))
      oos.writeObject(nodes)
      oos.close()
    }, "Saving Sorted Nodes to File")
  }

  /**
    * Load a priorityqueue of nodes from a file
    * @param filename to load the priorityqueue from
    * @return Priorityqueue of nodes, with their original ordering (if made using the HuffmanTree trait: ordered by frequency in reverse)
    */
  protected def loadSortedNodesFromFile(filename: String): mutable.PriorityQueue[Node] = {
    measureTime({
      val ois = new ObjectInputStream(new FileInputStream(filename))
      val o = ois.readObject().asInstanceOf[mutable.PriorityQueue[Node]]
      ois.close()
      o
    }, "Loading Sorted Nodes from File")
  }
}