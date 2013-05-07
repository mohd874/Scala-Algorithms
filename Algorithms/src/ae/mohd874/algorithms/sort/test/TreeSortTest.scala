package ae.mohd874.algorithms.sort.test

import ae.mohd874.algorithms.sort._
import scala.util.Random

object TreeSortTest extends App{
  
  //Created unsorted lists
  val words = List("revertively", "dispelled", "overmoral",
    "sylphid", "nonhabitability", "noiselessness",
    "undisconnected", "shoveling", "visalia", "ilo");
  
  val numbers = Random.shuffle(((1 to 200) toList))
  
  //Define the comparison function
  def lessWords(x: String, y: String) = x < y
  
  def lessNumber(x: Int, y: Int) = x < y
  
  TreeSortTestExecutor.test(lessWords)(words)
  println("\n------------------------------\n")
  TreeSortTestExecutor.test(lessNumber)(numbers)
}

object TreeSortTestExecutor {
  def test [T](less: (T,T) => Boolean)(xs: List[T]){
    //Print the words before sorting
    val f = (A: T) => println(A);

    println("---- print List ----")
    xs.map(f);

    //Construct the tree and start sorting
    val x = Node(less)(xs);

    //Define the start point of recurseNode function
    def output[T](root: Node[T], recurse: (Node[T], Int) => Unit) = {
      recurse(root, 0)
    }

    def renderTree[T](root: Node[T]) = {
      output(root, TreeSortTreeRederer.recurseNode[T]);
    }

    println("---- render List (sorted) ----")
    renderTree(x);

    //Define start point for sortedRender
    def renderTreeSorted[T](root: Node[T]) = {
      output(root, TreeSortTreeRederer.sortedRender[T]);
    }

    println("---- print List (sorted)----")
    println("")
    renderTreeSorted(x);

  }
}

object TreeSortTreeRederer {
  //Define a function to recursively go through all the tree nodes and print them in tree structure
  def recurseNode[T](root: Node[T], depth: Int) {
    def display[T](data: T, depth: Int) {
      for (i <- 1 to depth * 2) {
        print("-")
      }
      println(data)
    }
    root match {
      case null => {
        display("[]", depth)
      }
      case LeafNode(data) => {
        display(data, depth)
        recurseNode(null, depth + 1)
        recurseNode(null, depth + 1)
      }
      case FullNode(data, left, right) => {
        display(data, depth)
        recurseNode(left, depth + 1)
        recurseNode(right, depth + 1)
      }
      case RightNode(data, right) => {
        display(data, depth)
        recurseNode(null, depth + 1)
        recurseNode(right, depth + 1)
      }
      case LeftNode(data, left) => {
        display(data, depth)
        recurseNode(left, depth + 1)
        recurseNode(null, depth + 1)
      }
    }
  }
  
  //Define a function to recursively go through all the tree nodes and print them 
  def sortedRender[T](A: Node[T], depth: Int) {
    def display[T](data: T, depth: Int) {
      println(data);
    }
    A match {
      case null => {
      }
      case LeafNode(data) => {
        display(data, depth)
      }
      case FullNode(data, left, right) => {
        sortedRender(left, depth + 1)
        display(data, depth)
        sortedRender(right, depth + 1)
      }
      case RightNode(data, right) => {
        sortedRender(null, depth + 1)
        display(data, depth)
        sortedRender(right, depth + 1)
      }
      case LeftNode(data, left) => {
        sortedRender(left, depth + 1)
        display(data, depth)
      }
    }
  }
}
