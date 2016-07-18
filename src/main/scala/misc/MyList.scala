
package misc

object MyList {
  sealed trait MyList[+A]
  case object Nil extends MyList[Nothing]
  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  def sum(list: MyList[Int]): Int = {
    list match {
      case Nil              => 0
      case Cons(head, tail) => head + sum(tail)
    }
  }
  def product(list: MyList[Double]): Double = {
    list match {
      case Nil              => 1.0
      case Cons(0.0, _)     => 0.0
      case Cons(head, tail) => head * product(tail)
    }
  }
  def sum2(list: MyList[Int]): Int = {
    def sumImpl(acc: Int, l: MyList[Int]): Int = {
      l match {
        case Nil              => acc
        case Cons(head, tail) => sumImpl(head + acc, tail)
      }
    }
    sumImpl(0, list)
  }
  def product2(list: MyList[Double]): Double = {
    def productImpl(acc: Double, l: MyList[Double]): Double = {
      l match {
        case Nil              => acc
        case Cons(head, tail) => productImpl(head * acc, tail)
      }
    }
    productImpl(1, list)
  }

  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](l1: MyList[A], l2: MyList[A]): MyList[A] = {
    l1 match {
      case Nil              => l2
      case Cons(head, tail) => Cons(head, append(tail, l2))
    }
  }
  /*
   * If your operator is right-associative (right fold), the parentheses would be set like this:
   * A x (B x (C x D))
   * 
   */
  def foldRight[A, B](l1: MyList[A], z: B)(f: (A, B) => B): B =
    {
      l1 match {
        case Nil        => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }
    }
  def sum3(list: MyList[Int]): Int = {
    foldRight(list, 0)((x, y) => x + y)
  }
  def product3(list: MyList[Double]): Double = {
    foldRight(list, 1.0)(_ * _)
  }
  def length[A](list: MyList[A]): Int = {
    foldRight(list, 0)((_, acc) => acc + 1)
  }

  /*
   * If you have a left-associative operator, you'll set the parentheses like this
   * ((A x B) x C) x D
   */
  @annotation.tailrec
  def foldLeft[A, B](list: MyList[A], z: B)(f: (B, A) => B): B =
    {
      list match {
        case Nil        => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }
    }
  def sum4(list: MyList[Int]): Int = {
    foldLeft(list, 0)(_ + _)
  }
  def product4(list: MyList[Double]): Double = {
    foldLeft(list, 1.0)(_ * _)
  }
  def length2[A](list: MyList[A]): Int = {
    foldLeft(list, 0)((acc, _) => acc + 1)
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    foldLeft(list, MyList[A]())((acc, h) => Cons(h, acc))
  }
  def print[A](list: MyList[A]): Unit = {
    list match {
      case Nil => println
      case Cons(h, t) => {
        Console.print(h)
        print(t)
      }
    }
  }
  def mkString[A](list: MyList[A], builder: StringBuilder): String = {
    list match {
      case Nil => builder.toString()
      case Cons(h, t) => {
        builder.append(h)
        mkString(t, builder)
      }
    }
  }
  def dropWhile[A](list: MyList[A], f: A => Boolean): MyList[A] = {
    list match {
      case Cons(h, t) if (f(h)) =>
        dropWhile(t, f)
      case _ =>
        list
    }
  }
  def drop[A](list: MyList[A], n: Int): MyList[A] = {
    if (n <= 0)
      list
    else {
      list match {
        case Nil        => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }

  }
  def filter[A](list: MyList[A], f: A => Boolean): MyList[A] = {
    //foldRight
    //f: (A, B) => B             
    foldRight(list, Nil: MyList[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }
  def startsWith[A](list1: MyList[A], list2: MyList[A]): Boolean = {
    (list1, list2) match {
      case (_, Nil)                                => true
      case (Cons(h, t), Cons(h1, t1)) if (h == h1) => startsWith(t, t1)
      case _                                       => false
    }
  }
  def hasSubsequence[A](list1: MyList[A], list2: MyList[A]): Boolean = {
    list1 match {
      case Nil                           => list2 == Nil
      case _ if startsWith(list1, list2) => true
      case Cons(h, t)                    => hasSubsequence(t, list2)
    }
  }
  def last[A](list: MyList[A]): A = {
    list match {
      case Cons(head, Nil) => head
      case Cons(_, tail)   => last(tail)
      case _               => throw new NoSuchElementException
    }
  }
  def lastNth[A](list: MyList[A], n: Int): A = {
    list match {
      case Cons(head, tail) if (length(tail) == n) => head
      case Cons(_, tail)                           => lastNth(tail, n)
    }
  }
  def member[A](list: MyList[A], x: A): Boolean = {
    list match {
      case Nil              => false
      case Cons(head, tail) => (head == x) || member(tail, x)
    }
  }
  def add[A](list: MyList[A], x: A): MyList[A] = {
    if (member(list, x)) list else Cons(x, list)
  }
  def union[A](list1: MyList[A], list2: MyList[A]): MyList[A] = {
    list1 match {
      case Nil              => list2
      case Cons(head, tail) => add(union(tail, list2), head)
    }
  }
  def intersect[A](list1: MyList[A], list2: MyList[A]): MyList[A] = {
    list1 match {
      case Nil              => Nil
      case Cons(head, tail) => if (member(list2, head)) Cons(head, intersect(tail, list2)) else intersect(tail, list2)
    }
  }
  def subset[A](list1:MyList[A], list2:MyList[A]):Boolean={
    list1 match{
      case Nil => true
      case Cons(head,tail) => member(list2, head) && subset(tail, list2)
    }
  }
}