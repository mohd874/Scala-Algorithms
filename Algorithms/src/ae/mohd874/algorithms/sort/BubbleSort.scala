package ae.mohd874.algorithms.sort

object BubbleSort {

  def apply[T: ClassManifest](less: (T, T) => Boolean)(xs: Array[T]) = {
    bubbleSort(less)(xs).toArray
  }

  def bubbleSort[T: ClassManifest](less: (T, T) => Boolean)(xs: Array[T]): Stream[T] = {
    var swapped = false
    val size = xs.size

    if (size <= 1) {
      xs.toStream
    } else {
      def swapWithPrevious(i: Int, xs: Array[T]) = {
        val t = xs(i)
        xs(i) = xs(i - 1)
        xs(i - 1) = t
      }
      for (i <- size - 1 until 0 by -1) {
        if (less(xs(i), xs(i - 1))) {
          swapWithPrevious(i, xs)
          swapped = true
        }
      }
      if (swapped) {
        Stream.cons(xs.head, bubbleSort(less)(xs.tail))
      } else {
        xs.toStream
      }
    }
  }
}