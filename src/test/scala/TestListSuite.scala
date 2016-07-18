import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestListSuite extends FunSuite {
  import misc.MyList._
  test("sum: test list sum") {
    assert(sum(Cons(1, Cons(2, Cons(3, Nil)))) === 6)
  }
  test("product: test list product") {
    assert(product(Cons(1, Cons(2, Cons(3, Nil)))) === 6)
  }
  test("sum2: test list sum2") {
    assert(sum2(Cons(3, Cons(4, Cons(5, Nil)))) === 12)
  }
  test("product2: test list product2") {
    assert(product2(Cons(3, Cons(4, Cons(5, Nil)))) === 60)
  }
  test("sum3: test list sum3") {
    assert(sum3(Cons(3, Cons(4, Cons(5, Nil)))) === 12)
  }
  test("product3: test list product3") {
    assert(product3(Cons(3, Cons(4, Cons(5, Nil)))) === 60)
  }
  test("length: test list length") {
    assert(length(Cons(1, Cons(2, Cons(3, Nil)))) === 3)
  }

  test("append: test list append") {
    val list = append(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Nil))))
    assert(length(list) === 6)
  }
  test("sum4: test list sum4") {
    assert(sum4(Cons(3, Cons(4, Cons(5, Nil)))) === 12)
  }
  test("product4: test list product4") {
    assert(product4(Cons(3, Cons(4, Cons(5, Nil)))) === 60)
  }
  test("length2: test list length2") {
    assert(length2(Cons(1, Cons(2, Cons(3, Nil)))) === 3)
  }
  test("reverse: test list reverse") {
    // "%s".format(print(Cons(1, Cons(2, Cons(3, Nil)))))
    //"%s".format(print(reverse(Cons(1, Cons(2, Cons(3, Nil))))))
    var builder = StringBuilder.newBuilder
    mkString(Cons(1, Cons(2, Cons(3, Nil))), builder)
    assert(builder.toString === "123")
    builder = StringBuilder.newBuilder
    mkString(reverse(Cons(1, Cons(2, Cons(3, Nil)))), builder)
    assert(builder.toString === "321")
  }
  test("drop: test list drop") {
    val list = Cons(1, Cons(2, Cons(3, Nil)))
    assert(length(drop(list, 1)) === 2)
  }

  test("dropWhile: test list dropWhile") {
    val list = Cons(2, Cons(1, Cons(3, Nil)))
    assert(length(dropWhile[Int](list, (x: Int) => x / 2 == 1)) === 2)
  }
  test("filter: test list filter") {
    val list = Cons(1, Cons(2, Cons(3, Nil)))
    assert(length(filter[Int](list, (x: Int) => x / 2 == 1)) === 2)
  }
  test("hasSubsequence: test list hasSubsequence") {
    val list1 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    val list2 = Cons(2, Cons(3, Cons(4, Nil)))
    assert(hasSubsequence[Int](list1, list2) === true)
  }
  test("last: test list last element") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    assert(last[Int](list) === 5)
  }
  test("lastNth: test list last Nth element") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    assert(lastNth[Int](list, 2) === 3)
  }
  test("member: test list member") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    assert(member[Int](list, 2) === true)
  }
  test("add: test list add") {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    val list2 = Cons(6, Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
    assert(add[Int](list, 6) === list2)
  }
  test("union: test list union") {
    val list = Cons(1, Cons(2, Cons(3, Cons(7, Cons(8, Nil)))))
    val list2 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    val list3 = Cons(7, Cons(8, Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))))
    assert(union[Int](list, list2) === list3)
  }
test("intersect: test list intersect") {
    val list = Cons(1, Cons(2, Cons(3, Cons(7, Cons(8, Nil)))))
    val list2 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    val list3 = Cons(1, Cons(2, Cons(3, Nil)))
    assert(intersect[Int](list, list2) === list3)
  }
test("subset: test list subset") {
    val list = Cons(1, Cons(2, Cons(3, Cons(7, Cons(8, Nil)))))
    val list2 = Cons(1, Cons(2, Cons(3, Cons(7, Cons(8, Cons(6, Nil))))))
    assert(subset[Int](list, list2) === true)
  }
}