package tree

import scala.util.Sorting
import scala.collection.mutable.ListBuffer

object HuffmanTree {

  abstract sealed class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {

    case Leaf(_, weight)       => weight
    case Fork(_, _, _, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, _)        => List(char)
    case Fork(_, _, chars, _) => chars
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def times0(chars: List[Char], map: scala.collection.mutable.Map[Char, Int]): Unit =
      {
        chars match {
          case Nil => Nil
          case head :: tail =>
            val prevCount = if (map.contains(head)) map(head)
            else 0
            map += (head -> (prevCount + 1))
            times0(tail, map)
        }
      }
    var map: scala.collection.mutable.Map[Char, Int] = scala.collection.mutable.Map()
    times0(chars, map)

    var acc: ListBuffer[(Char, Int)] = ListBuffer()
    map.foreach { case (k, v) => acc.append((k, v)) }
    acc.toList
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    {
      freqs.map(e => Leaf(e._1, e._2)).sortBy((leaf: Leaf) => leaf.weight)
    }

  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case t :: Nil => true
    case _        => false
  }
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil         => Nil
    case head :: Nil => head :: Nil
    case left :: right :: tail =>
      val t1 = makeCodeTree(left, right)
      val lstPartition = tail.partition(z => weight(z) <= weight(t1))
      (lstPartition._1 ::: t1 :: lstPartition._2)
  }

  def until(fsingleton: List[CodeTree] => Boolean, fcombine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (fsingleton(trees)) trees
    else until(fsingleton, fcombine)(fcombine(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def chooseBranch(tree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = {
      tree match {
        case leaf: Leaf => (leaf.char, bits)
        case fork: Fork => {
          if (bits.head == 0)
            chooseBranch(fork.left, bits.tail)
          else
            chooseBranch(fork.right, bits.tail)
        }
      }
    }
    def decode1(tree: CodeTree, bits: List[Bit], txtDecode: List[Char]): List[Char] = {
      if (bits.isEmpty) txtDecode
      else {
        val nextBranch = chooseBranch(tree, bits)
        val c = nextBranch._1
        val bitsTail = nextBranch._2
        c :: decode1(tree, bitsTail, txtDecode)
      }
    }
    decode1(tree, bits, Nil)
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {

    def encodeSymbol(currentTree: CodeTree, ch: Char, acc: List[Bit]): List[Bit] = currentTree match {
      case l: Leaf => if (ch == l.char) acc else Nil
      case f: Fork => encodeSymbol(f.left, ch, acc :+ 0) ::: encodeSymbol(f.right, ch, acc :+ 1)
    }

    def encode1(tree: CodeTree, text: List[Char], bits: List[Bit]): List[Bit] = {
      if (text.isEmpty) bits
      else encodeSymbol(tree, text.head, bits) ::: encode1(tree, text.tail, bits)
    }

    encode1(tree, text, Nil)
  }

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] =
    table.find(x => x._1 == char) match {
      case Some(x) => x._2
      case None    => Nil
    }

  def convert(tree: CodeTree): CodeTable = {
    def loop(cur: CodeTree, table: CodeTable, bits: List[Bit]): CodeTable = {

      cur match {
        case l: Leaf => (l.char, bits) :: table
        case f: Fork => mergeCodeTables(loop(f.left, table, bits :+ 0), loop(f.right, table, bits :+ 1))
      }
    }
    loop(tree, Nil, Nil)
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)
    def loop(text: List[Char], bits: List[Bit]): List[Bit] = {
      if (text.isEmpty) bits
      else codeBits(codeTable)(text.head) ::: loop(text.tail, bits)
    }
    loop(text, Nil)
  }
}
