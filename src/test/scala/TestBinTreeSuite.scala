
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import tree.BinTree

@RunWith(classOf[JUnitRunner])
class TestBinTreeSuite extends FunSuite {
  import BinTree._

  test("size: test tree size") {
    val t = insert(List(6, 2, 8, 1, 4, 3, 7))
    assert(size(t) === 7)
  }
  test("addAll: test tree addAll") {
    val t = addAll(List(6, 2, 8, 1, 4, 3, 7))
    assert(size(t) === 7)
  }
  test("depth: test tree depth") {
    val t = insert(List(6, 2, 8, 1, 4, 3, 7))
    assert(depth(t) === 4)
  }
  test("depth: test tree depth1") {
    val t = insert(List(35, 21, 70, 1, 68, 72, 60, 62))
    assert(depth(t) === 5)
  }
  test("depth: test tree depth2") {
    val t = addAll(List(35, 21, 70, 1, 68, 72, 60, 62))
    assert(depth(t) === 5)
  }
  test("depth: test tree depth3") {
    val t = addAll(List(50, 25, 75, 10, 30, 60, 90, 4, 12, 27, 40, 55, 65, 80, 99))
    assert(depth(t) === 4)
  }
  test("contains: test tree contains") {
    val t = insert(List(6, 2, 8, 1, 4, 3, 7))
    assert(contains(t)(8) === true)
  }
  test("contains: test tree contains1") {
    val t = insert(List(35, 21, 70, 1, 68, 72, 60, 62))
    assert(contains(t)(72) === true)
  }
  test("findMin: test tree minimum") {
    val t = insert(List(35, 21, 70, 1, 68, 72, 60, 62))
    assert(findMin(t) === 1)
  }
  test("findMin: test tree minimum1") {
    val t = insert(List(6, 2, 8, 1, 4, 3, 7))
    assert(findMin(t) === 1)
  }
  test("findMin: test tree maximum") {
    val t = insert(List(35, 21, 70, 1, 68, 72, 60, 62))
    assert(findMax(t) === 72)
  }
  test("findMin: test tree maximum1") {
    val t = insert(List(6, 2, 8, 1, 4, 3, 7))
    assert(findMax(t) === 8)
  }
  test("foreach: test tree foreach") {
    // val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val t = insert(List(4, 2, 6, 1, 5, 3, 7))
    foreach(t)(print)
    println
  }
  test("inorder: test tree inorder") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = inorder(t)
    assert(tree === List(1, 2, 3, 4, 5, 6, 7))
  }
  test("preorder: test tree preorder") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = preorder(t)
    assert(tree === List(4, 2, 1, 3, 6, 5, 7))
  }
  test("postorder: test tree postorder") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = postorder(t)
    assert(tree === List(1, 3, 2, 5, 7, 6, 4))
  }
  test("depthFirstSearch: test tree depthFirstSearch") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = depthFirstSearch(t)
    assert(tree === List(4, 2, 1, 3, 6, 5, 7))
  }

  test("breadthFirstSearch: test tree breadthFirstSearch") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(t)
    assert(tree === List(4, 2, 6, 1, 3, 5, 7))
  }
  test("levelOrderSearch: test tree levelOrderSearch") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = levelOrderSearch(t)
    assert(tree === List(4, 2, 6, 1, 3, 5, 7))
  }
  test("delete: test tree delete") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(delete(t)(3))
    assert(tree === List(4, 2, 6, 1, 5, 7))
  }
  test("delete: test tree delete1") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(delete(t)(1))
    assert(tree === List(4, 2, 6, 3, 5, 7))
  }
  test("delete: test tree delete2") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(delete(t)(2))
    assert(tree === List(4, 3, 6, 1, 5, 7))
  }
  test("delete: test tree delete3") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(delete(t)(7))
    assert(tree === List(4, 2, 6, 1, 3, 5))
  }
  test("delete: test tree delete4") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(delete(t)(5))
    assert(tree === List(4, 2, 6, 1, 3, 7))
  }
  test("delete: test tree delete5") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(delete(t)(6))
    assert(tree === List(4, 2, 7, 1, 3, 5))
  }
  test("delete: test tree delete6") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(delete(t)(4))
    assert(tree === List(5, 2, 6, 1, 3, 7))
  }
 test("remove: test tree remove") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(remove(t)(3))
    assert(tree === List(4, 2, 6, 1, 5, 7))
  }
  test("remove: test tree remove1") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(remove(t)(1))
    assert(tree === List(4, 2, 6, 3, 5, 7))
  }
  test("remove: test tree remove2") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(remove(t)(2))
    assert(tree === List(4, 3, 6, 1, 5, 7))
  }
  test("remove: test tree remove3") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(remove(t)(7))
    assert(tree === List(4, 2, 6, 1, 3, 5))
  }
  test("remove: test tree remove4") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(remove(t)(5))
    assert(tree === List(4, 2, 6, 1, 3, 7))
  }
  test("remove: test tree remove5") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(remove(t)(6))
    assert(tree === List(4, 2, 7, 1, 3, 5))
  }
  test("remove: test tree remove6") {
    val t = Node(4, Node(2, Node(1, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(6, Node(5, EmptyTree, EmptyTree), Node(7, EmptyTree, EmptyTree)))
    val tree = breadthFirstSearch(remove(t)(4))
    assert(tree === List(5, 2, 6, 1, 3, 7))
  }
}
