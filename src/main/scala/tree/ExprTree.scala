//
//datatype BinTree = Empty | Node(Int, BinTree, BinTree)
//abstract class BinTree
//case object EmptyTree extends BinTree
//case class Node(elem : Int, left : BinTree,right: BinTree) extends BinTree
//
//

package tree

object Expr {
  sealed abstract class Expr
  case class Var(name: String) extends Expr
  case class Number(num: Double) extends Expr
  case class UnOp(operator: String, arg: Expr) extends Expr
  case class BinOp(operator: String, left: Expr, right: Expr) extends Expr
  def eval(e: Expr): Double = (e: @unchecked) match {
    case Number(i) => i
    case BinOp(op, e1, e2) => op match {
      case "+" => eval(e1) + eval(e2)
      case "-" => eval(e1) - eval(e2)
      case "*" => eval(e1) * eval(e2)
      case "/" => eval(e1) - eval(e2)
    }
  }
}