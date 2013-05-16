package ae.mohd874.algorithms.sort

object SelectionSort {

  def apply[T: ClassManifest](less: (T, T) => Boolean)(xs: Array[T]) = {
    selectionSort(less)(xs).toArray
  }

  def selectionSort[T: ClassManifest](less: (T, T) => Boolean)(xs: Array[T]): Stream[T] = {
    var swapped = false
    val size = xs.size
    var min = size - 1

    if (size <= 1) {
      xs.toStream
    } else {
      def swap(i: Int, j: Int, xs: Array[T]) = {
        val t = xs(i)
        xs(i) = xs(j)
        xs(j) = t
      }
      for (i <- size - 1 until 0 by -1) {
        if (less(xs(i), xs(min))) {
          min = i
        }
      }
      swap(0, min, xs)
      Stream.cons(xs.head, selectionSort(less)(xs.tail))
    }
  }
}