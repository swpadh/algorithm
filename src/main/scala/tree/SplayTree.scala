package tree

object SplayTree {
  sealed abstract class SplayTree
  case object EmptyTree extends SplayTree
  case class Node(elem: Int, left: SplayTree, right: SplayTree) extends SplayTree
  def partition(pivot: Int, t: SplayTree): (SplayTree, SplayTree) = {
    t match {

      case EmptyTree => (EmptyTree, EmptyTree)
      case Node(x, a, b) => if (x <= pivot) {
        b match {
          case EmptyTree => (t, EmptyTree)
          case Node(y, b1, b2) => if (y <= pivot) {
            val (small, big) = partition(pivot, b2)
            (Node(y, Node(x, a, b1), small), big)
          } else {
            val (small, big) = partition(pivot, b1)
            (Node(x, a, small), Node(y, big, b2))
          }
        }
      } else {
        a match {
          case EmptyTree => (EmptyTree, t)
          case Node(y, a1, a2) => if (y <= pivot) {
            val (small, big) = partition(pivot, a2)
            (Node(y, a, small), Node(x, big, b))
          } else {
            val (small, big) = partition(pivot, a1)
            (small, Node(y, big, Node(x, a2, b)))
          }
        }
      }
    }
  }

  def insert(x: Int, t: SplayTree): SplayTree = {
    val (a, b) = partition(x, t)
    Node(x, a, b)
  }
  def merge(t1: SplayTree, t: SplayTree): SplayTree = {
    t1 match {
      case EmptyTree => t
      case Node(x, a, b) =>
        val (ta, tb) = partition(x, t)
        Node(x, merge(ta, a), merge(tb, b))
    }
  }
  def findMin(t: SplayTree): Int = {
    t match {
      case EmptyTree             => throw new Exception("empty tree")
      case Node(x, EmptyTree, b) => x
      case Node(x, a, b)         => findMin(a)
    }
  }
  def deleteMin(t: SplayTree): SplayTree = {
    t match {
      case EmptyTree                         => throw new Exception("empty tree")
      case Node(x, EmptyTree, b)             => b
      case Node(y, Node(x, EmptyTree, b), c) => Node(y, b, c)
      case Node(y, Node(x, a, b), c)         => Node(x, deleteMin(a), Node(y, b, c))
    }
  }
}