import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import tree._

@RunWith(classOf[JUnitRunner])
class TestRedBlackTreeSuite extends FunSuite {
  import RedBlackTree._
  test("depth: test tree depth") {
    val t = addAll(List(4, 2, 6, 1, 3, 5, 7))
   // assert(depth(t) === 3)
  }
  test("inorder: test tree inorder") {
    val t = addAll(List(1,2,4,5,7,8,11,14,15))
    assert(inorder(t) ===List((1,Black), (2,Black),(4,Black),(5,Black),(7,Black),(8,Black),(11,Black),(14,Red),(15,Black)))
   }
   test("levelorder: test tree levelorder") {
    val t = insert(List(7,2,11,1,5,8,14,4,15))
    assert(breadthFirstSearch(t) === List((5,Black), (2,Black), (14,Black), (1,Black), (4,Black), (8,Black), (15,Black), (7,Red), (11,Red)))
  }
  
  test("breadthFirstSearch: test tree breadthFirstSearch") {
    import RedBlackTree._
     val left = Node(2, Red, Node(1, Black, EmptyTree, EmptyTree), Node(5, Black, Node(4, Red, EmptyTree, EmptyTree), EmptyTree))
    val right = Node(11, Red, Node(8, Black, EmptyTree, EmptyTree), Node(14, Black, EmptyTree, Node(15, Red, EmptyTree, EmptyTree)))
    val t = Node(7, Black,left,right)
    val tree = breadthFirstSearch(t)
    assert(tree === List((7,Black), (2,Red),(11,Red),(1,Black),(5,Black),(8,Black),(14,Black), (4,Red),(15,Red)))
  }
  test("depthFirstSearch: test tree depthFirstSearch") {
    import RedBlackTree._
    val left = Node(2, Red, Node(1, Black, EmptyTree, EmptyTree), Node(5, Black, Node(4, Red, EmptyTree, EmptyTree), EmptyTree))
    val right = Node(11, Red, Node(8, Black, EmptyTree, EmptyTree), Node(14, Black, EmptyTree, Node(15, Red, EmptyTree, EmptyTree)))
    val t = Node(7, Black,left,right)
    val tree = depthFirstSearch(t)
    assert(tree === List((7,Black), (2,Red),(1,Black),(5,Black), (4,Red), (11,Red), (8,Black),(14,Black), (15,Red)))
  }

}