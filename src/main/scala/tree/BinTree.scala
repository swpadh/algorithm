//
//datatype BinTree = Empty | Node(Int, BinTree, BinTree)
//
package tree

object BinTree {

  sealed abstract class BinTree
  case object EmptyTree extends BinTree
  case class Node(elem: Int, left: BinTree, right: BinTree) extends BinTree
  def insert(x: Int, t: BinTree): BinTree = {
    t match {
      case EmptyTree => Node(x, EmptyTree, EmptyTree)
      case s @ Node(y, left, right) =>
        val c = x compare y
        if (c == 0)
          s
        else if (c < 0)
          Node(y, insert(x, left), right)
        else
          Node(y, left, insert(x, right))
    }
  }

  def insert(list: List[Int]): BinTree = {
    list match {
      case Nil          => EmptyTree
      case head :: tail => insert(head, insert(tail))
    }
  }
  def addAll(list: List[Int]): BinTree = {
    list.foldLeft(EmptyTree: BinTree)((t: BinTree, x: Int) => insert(x: Int, t: BinTree))
  }
  def size(t: BinTree): Int = {
    t match {
      case EmptyTree            => 0
      case Node(y, left, right) => 1 + size(left) + size(right)
    }
  }
  def depth(t: BinTree): Int = {
    t match {
      case EmptyTree            => 0
      case Node(y, left, right) => 1 + (depth(left) max depth(right))
    }
  }
  def contains(t: BinTree)(x: Int): Boolean = {
    t match {
      case EmptyTree => false
      case Node(y, left, right) =>
        val c = x compare y
        if (c == 0) true
        else if (c > 0)
          contains(right)(x)
        else
          contains(left)(x)
    }
  }
  def findMin(t: BinTree): Int = {
    t match {
      case EmptyTree => Int.MinValue
      case Node(y, left, right) => if (left == EmptyTree)
        y
      else findMin(left)
    }
  }
  def findMax(t: BinTree): Int = {
    t match {
      case EmptyTree => Int.MaxValue
      case Node(y, left, right) => if (right == EmptyTree)
        y
      else findMax(right)
    }
  }

  def delete(t: BinTree)(x: Int): BinTree = {
    t match {
      case EmptyTree => EmptyTree
      case Node(y, left, right) =>
        val c = x compare y
        if (c == 0) { //Found the Element to be deleted
          if (left == EmptyTree) { //Only a right child
            right
          } else if (right == EmptyTree) { //Only a left child
            left
          } else { //2 children. replace with smallest in right subtree (successor)
            val tmp = findMin(right)
            Node(tmp, left, delete(right)(tmp))
          }
        } else if (c < 0)
          Node(y, delete(left)(x), right)
        else
          Node(y, left, delete(right)(x))
    }
  }

  def remove(t: BinTree)(x: Int): BinTree = {
    def removeMin(t: BinTree): BinTree = {
      t match {
        case EmptyTree                 => EmptyTree
        case Node(y, EmptyTree, right) => right
        case Node(y, left, right) =>
          Node(y, removeMin(left), right)
      }
    }
    t match {
      case EmptyTree => EmptyTree
      case Node(y, left, right) =>
        val c = x compare y
        if (c == 0) { //Found the Element to be deleted
          if (right == EmptyTree) { //Only a left child
            left
          } else if (left == EmptyTree) { //Only a right child
            right
          } else { //2 children. replace with smallest in right subtree  (successor)
            val tmp = findMin(right)
            Node(tmp, left, removeMin(right))
          }
        } else if (c < 0) {
          Node(y, remove(left)(x), right)
        } else {
          Node(y, left, remove(right)(x))
        }
    }

  }
  def foreach(t: BinTree)(f: Int => Unit): Unit = {
    t match {
      case EmptyTree => EmptyTree
      case Node(y, left, right) =>
        foreach(left)(f)
        f(y)
        foreach(right)(f)
    }
  }
  def inorder(t: BinTree): List[Int] = {
    t match {
      case EmptyTree => Nil
      case Node(y, left, right) =>
        inorder(left) ::: List(y) ::: inorder(right)
    }
  }
  def preorder(t: BinTree): List[Int] = {
    t match {
      case EmptyTree => Nil
      case Node(y, left, right) =>
        List(y) ::: preorder(left) ::: preorder(right)
    }
  }
  def postorder(t: BinTree): List[Int] = {
    t match {
      case EmptyTree => Nil
      case Node(y, left, right) =>
        postorder(left) ::: postorder(right) ::: List(y)
    }
  }
  def depthFirstSearch(t: BinTree): List[Int] = {
    def loop(s: List[BinTree]): List[Int] = {
      s match {
        case Nil => Nil
        case head :: tail => if (head == EmptyTree)
          loop(tail)
        else {
          val h = head.asInstanceOf[Node]
          //PreOrder Traversal
          h.elem :: loop(h.left :: h.right :: tail)
        }
      }
    }
    loop(List(t))
  }

  def levelOrderSearch(t: BinTree): List[Int] =
    {
      breadthFirstSearch(t)
    }
  def breadthFirstSearch(t: BinTree): List[Int] = {
    import scala.collection.immutable.Queue
    def loop(q: Queue[BinTree]): List[Int] = {
      if (q.isEmpty)
        Nil
      else if (q.head == EmptyTree)
        loop(q.tail)
      else {
        val h = q.head.asInstanceOf[Node]
        h.elem :: loop(q.tail :+ h.left :+ h.right)
      }
    }
    loop(Queue(t))
  }
}