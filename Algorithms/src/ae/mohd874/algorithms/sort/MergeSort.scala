package ae.mohd874.algorithms.sort

object MergeSort {

  def apply[T](less: (T,T) => Boolean)(xs: List[T]) = {
    msort(less)(xs)
  }

  /*
   * Source: http://stackoverflow.com/questions/2201472/merge-sort-from-programming-scala-causes-stack-overflow
   */

  def msort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
    def merge(left: List[T], right: List[T]): Stream[T] = (left, right) match {
      case (x :: xs, y :: ys) if less(x, y) => Stream.cons(x, merge(xs, right))
      case (x :: xs, y :: ys) => Stream.cons(y, merge(left, ys))
      case _ => if (left.isEmpty) right.toStream else left.toStream
    }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msort(less)(ys), msort(less)(zs)).toList
    }
  }

  def msort2[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T], acc: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys.reverse ::: acc
        case (_, Nil) => xs.reverse ::: acc
        case (x :: xs1, y :: ys1) =>
          if (less(x, y)) merge(xs1, ys, x :: acc)
          else merge(xs, ys1, y :: acc)
      }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msort(less)(ys), msort(less)(zs), Nil).reverse
    }
  }
  /*
   * 
   * My attempt to create MergeSort
   * 
  private def mergeSort(xs: List[Int]): List[Int] = {
    if (xs.size <= 1) {
      xs
    } else {
      val half = (xs size) / 2
      val (upper, lower) = xs splitAt (half)
      def merge(xs: List[Int], ys: List[Int]): List[Int] = {
        (xs, ys) match {
          case (x :: xz, Nil) => x :: xz
          case (Nil, y :: yz) => y :: yz
          case (x :: xz, y :: yz) => if (x < y) x :: merge(y :: yz, xz) else y :: merge(x :: xz, yz)
        }
      }
      merge(mergeSort(upper), mergeSort(lower))
    }
  }
  */
}