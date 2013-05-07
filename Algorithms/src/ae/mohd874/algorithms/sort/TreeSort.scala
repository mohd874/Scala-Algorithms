package ae.mohd874.algorithms.sort

object TreeSort {

}

/*
 * This is an example of Tree Sort.
 * 
 * The original code was written by Gary Sieling (see link below). 
 * 
 * Original Source Code: <a href='http://architects.dzone.com/articles/scala-tree-sort-example'> http://architects.dzone.com/articles/scala-tree-sort-example </a>
 * 
 * I've changed the code so that to make it more generic and reusable
 * 
 * To see the tests, please check ae.mohd874.algorithms.sort.TreeSortTest.scala
 */
object Node {

  def apply[T](less: (T, T) => Boolean)(xs: List[T]) = {
    sort(less)(xs)
  }

  def sort[T](less: (T, T) => Boolean)(xs: List[T]): Node[T] = {
    var tree: Node[T] = null;

    for (item <- xs) {
      tree = insert(less)(tree, item)
    }

    return tree
  }

  def insert[T](less: (T, T) => Boolean)(tree: Node[T], value: T): Node[T] = {
    tree match {
      case null => LeafNode(value)
      case LeafNode(data) => if (less(data, value)) {
        LeftNode(data, LeafNode(value))
      } else {
        RightNode(data, LeafNode(value))
      }
      case LeftNode(data, left) => if (less(data, value)) {
        LeftNode(value, LeftNode(data, left))
      } else {
        FullNode(data, left, LeafNode(value))
      }
      case RightNode(data, right) => if (less(data, value)) {
        FullNode(data, LeafNode(value), right)
      } else {
        RightNode(value, RightNode(data, right))
      }
      case FullNode(data, left, right) => if (less(data, value)) {
        FullNode(data, insert(less)(left, value), right)
      } else {
        FullNode(data, left, insert(less)(right, value))
      }
    }
  }
}

abstract class Node[T];
case class LeafNode[T](data: T) extends Node[T];
case class FullNode[T](data: T, left: Node[T], right: Node[T]) extends Node[T]
case class LeftNode[T](data: T, left: Node[T]) extends Node[T]
case class RightNode[T](data: T, right: Node[T]) extends Node[T]

