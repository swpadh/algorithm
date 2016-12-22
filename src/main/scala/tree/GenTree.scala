package tree

object GenTree {

  sealed abstract class GenTree
  case object EmptyTree extends GenTree
  case class Node(elem: Int, list: List[GenTree]) extends GenTree

  def size(t: GenTree): Int = {
    t match {
      case EmptyTree     => 0
      case Node(y, list) => 1 + (list.map(size).sum)
    }
  }
  def it_list[A](f: (A, A) => A, e: A, l: List[A]): A =
    {
      l match {
        case Nil          => e
        case head :: tail => it_list(f, f(e, head), tail)
      }
    }
  def list_it[A](f: (A, A) => A, l: List[A], e: A): A =
    {
      l match {
        case Nil          => e
        case head :: tail => f(head, list_it(f, tail, e))
      }
    }
  def max(a: Int, b: Int): Int = {
    if (a > b) a else b
  }
   def max(a: GenTree, b: GenTree): GenTree = {
     
     if(a == EmptyTree && b == EmptyTree)
       EmptyTree
     else if(a == EmptyTree)
       b
     else if(b== EmptyTree)
       a
       
     val ag = a.asInstanceOf[Node];
     val bg = b.asInstanceOf[Node];
     
    if (ag.elem> bg.elem) ag else bg
  }
  def height(t: GenTree): Int = {
    t match {
      case EmptyTree     => 0
      case Node(y, list) => 1 + list.map(height).fold(0)((a, b) => if (a > b) a else b)
    }
  }
  def flat(t: GenTree): List[Any] =
    {
      t match {
        case EmptyTree     => List()
        case Node(x, list) => x :: list.flatMap(flat)
      }
    }
  
}