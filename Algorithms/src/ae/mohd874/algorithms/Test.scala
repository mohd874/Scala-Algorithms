package ae.mohd874.algorithms

import scala.util.Random
import sort._

object Test extends App {

  //A small test
  val nums = List(4, 5, 8, 1, 7)

  //A challenge test
  val shuffled = Random.shuffle(((1 to 6000) toList))

  //Define comparing function
  def less(x: Int, y: Int) = x < y //> less: (x: Int, y: Int)Boolean

  def sortTest[T: ClassManifest](less: (T, T) => Boolean)(xs: Array[T], sortName: String) {
    //Some algorithms requires a list
    val xss = List.fromArray(xs)

    val time = System.nanoTime()
    sortName match {
      case "BubbleSort" => {
        BubbleSort(less)(xs)
      }
      case "SelectionSort" => {
        SelectionSort(less)(xs)
      }
      case "MergeSort" => MergeSort(less)(xss)
      case "TreeSort" => TreeSort(less)(xss)
      case "InsertionSort" => InsertionSort(less)(xss)
      case "QuickSort" => QuickSort(less)(xss)
    }
    val elapsed = System.nanoTime() - time

    printReport(sortName, elapsed, xs)
  }

  def nanoToSec(nano: Long): Float = {
    val res = nano / 100000000F
    res
  }

  def printReport[T](sortName: String, elapsed: Long, xs: Array[T]) {
    println("Algorithm: " + sortName)
    println("List/Array size: " + xs.length)
    println("Time Elapsed: " + nanoToSec(elapsed))
    println("---------------------\n")
  }

  println("--------- START EASY TEST --------- ")
  
  sortTest(less)(nums.toArray, "BubbleSort")
  sortTest(less)(nums.toArray, "MergeSort")
  sortTest(less)(nums.toArray, "SelectionSort")
  sortTest(less)(nums.toArray, "TreeSort")
  sortTest(less)(nums.toArray, "InsertionSort")
  sortTest(less)(nums.toArray, "QuickSort")

  println("--------- EASY TEST FINISHED--------- ")
  println("--------- START HARD TEST --------- ")
  
  sortTest(less)(shuffled.toArray, "BubbleSort")
  sortTest(less)(shuffled.toArray, "MergeSort")
  sortTest(less)(shuffled.toArray, "SelectionSort")
  sortTest(less)(shuffled.toArray, "TreeSort")
  sortTest(less)(shuffled.toArray, "InsertionSort")
  sortTest(less)(shuffled.toArray, "QuickSort")
  
  println("--------- TEST HARD FINISHED--------- ")
}