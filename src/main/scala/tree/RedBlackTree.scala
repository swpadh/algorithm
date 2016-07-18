package tree

object RedBlackTree {
  //
  //datatype Color R|B
  //
  sealed abstract class Color
  case object Red extends Color
  case object Black extends Color

  //
  //datatype Tree E | T of Elem x Color x Tree x Tree
  //
  sealed abstract class RedBlackTree
  case object EmptyTree extends RedBlackTree
  case class Node(elem: Int, color: Color, left: RedBlackTree, right: RedBlackTree) extends RedBlackTree

  def contains(t: RedBlackTree)(x: Int): Boolean = {
    t match {
      case EmptyTree => false
      case Node(y, _, left, right) =>
        val c = x compare y
        if (c == 0) true
        else if (c > 0)
          contains(right)(x)
        else
          contains(left)(x)
    }
  }

  def insert(x: Int, s: RedBlackTree): RedBlackTree = {
    def makeBlack(t: RedBlackTree): RedBlackTree = {
      t match {
        case EmptyTree        => EmptyTree
        case Node(y, _, a, b) => Node(y, Black, a, b)
      }
    }
    def insertImpl(rb: RedBlackTree): RedBlackTree = {
      rb match {
        case EmptyTree => Node(x, Red, EmptyTree, EmptyTree)
        case p @ Node(y, color, a, b) =>
          val c = x compare y
          if (c == 0) {
            p
          } else if (c < 0) {
            balance(Node(y, color, insertImpl(a), b))
          } else {
            balance(Node(y, color, a, insertImpl(b)))
          }
      }
    }
    makeBlack(insertImpl(s))
  }
  def balance(t: RedBlackTree): RedBlackTree = {
    t match {
      case Node(z, Black, Node(y, Red, Node(x, Red, a, b), c), d) =>
        Node(y, Red, Node(x, Black, a, b), Node(z, Black, c, d))
      case Node(z, Black, Node(x, Red, a, Node(y, Red, b, c)), d) =>
        Node(y, Red, Node(x, Black, a, b), Node(z, Black, c, d))
      case Node(x, Black, a, Node(z, Red, Node(y, Red, b, c), d)) =>
        Node(y, Red, Node(x, Black, a, b), Node(z, Black, c, d))
      case Node(x, Black, a, Node(y, Red, b, Node(z, Red, c, d))) =>
        Node(y, Red, Node(x, Black, a, b), Node(z, Black, c, d))
      case p => p
    }
  }
  def depth(t: RedBlackTree): Int = {
    t match {
      case EmptyTree               => 0
      case Node(_, _, left, right) => 1 + (depth(left) max depth(right))
    }
  }
  def breadthFirstSearch(t: RedBlackTree): List[(Int, Color)] = {
    import scala.collection.immutable.Queue
    def loop(q: Queue[RedBlackTree]): List[(Int, Color)] = {
      if (q.isEmpty)
        Nil
      else if (q.head == EmptyTree)
        loop(q.tail)
      else {
        val h = q.head.asInstanceOf[Node]
        (h.elem, h.color) :: loop(q.tail :+ h.left :+ h.right)
      }
    }
    loop(Queue(t))
  }
  def depthFirstSearch(t: RedBlackTree): List[(Int, Color)] = {
    def loop(s: List[RedBlackTree]): List[(Int, Color)] = {
      s match {
        case Nil => Nil
        case head :: tail => if (head == EmptyTree)
          loop(tail)
        else {
          val h = head.asInstanceOf[Node]
          //PreOrder Traversal
          (h.elem, h.color) :: loop(h.left :: h.right :: tail)
        }
      }
    }
    loop(List(t))
  }
  def inorder(t: RedBlackTree): List[(Int, Color)] = {
    t match {
      case EmptyTree => Nil
      case Node(y, color, left, right) =>
        inorder(left) ::: List((y, color)) ::: inorder(right)
    }
  }
  def insert(list: List[Int]): RedBlackTree = {
    list match {
      case Nil          => EmptyTree
      case head :: tail => insert(head, insert(tail))
    }
  }
  def addAll(list: List[Int]): RedBlackTree = {
    list.foldLeft(EmptyTree: RedBlackTree)((t: RedBlackTree, x: Int) => insert(x: Int, t: RedBlackTree))
  }
}