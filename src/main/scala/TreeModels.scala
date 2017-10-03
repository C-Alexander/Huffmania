import scala.collection.immutable.HashSet

//this is not how I wanted it, but the serializer is going f*** insane over having models from mixins. Limit of using Scala
//with java shit I guess. I need to go report a bug at Scala, NO way this behavior is intended.
//It doesn't even fucking give a proper error. I spent 2 hours trying to figure out WHAT the problem is.
//It outright gives false errors and then finally ADMITS it really just doesn't know...
//Generally any object in the scope should be fine, my guess is its fucking up with the scope when mixing traits.
object TreeModels {
  abstract class Node() {
    def frequency: Int
  }
  case class Leaf(char: Char, frequency: Int) extends Node  {
    override def toString: String = s"$frequency:${char.toString.replace("\n", "\\n")}"
  }
  case class Branch(left: Node, right: Node, charSet: HashSet[Char], frequency: Int) extends Node {
    // override def toString: String = s"| $frequency:${charSet.mkString("[", ",", "]")} |\n\t/\t\\\n/\t\t\\\n${left.toString}\t\t${right.toString}"
    override def toString: String = s"$frequency:${charSet.mkString("[", ",", "]").replace("\n", "\\n")}"
  }
  object Branch {
    def apply(left: Node, right: Node): Branch = {
      (left, right) match {
        case (left: Leaf, right: Leaf) => Branch(left.asInstanceOf[Leaf], right.asInstanceOf[Leaf])
        case (left: Leaf, right: Branch) => Branch(left.asInstanceOf[Leaf], right.asInstanceOf[Branch])
        case (left: Branch, right: Leaf) => Branch(left.asInstanceOf[Branch], right.asInstanceOf[Leaf])
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
