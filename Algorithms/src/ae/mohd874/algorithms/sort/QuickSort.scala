package ae.mohd874.algorithms.sort

/*
 * Source: Unknown
 * If you know the original author, please let me know.
 */
object QuickSort {
  def apply[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
	sort(less)(xs)
  }
  def sort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
    if (xs.length < 2)
      xs
    else {
      val pivot = xs(xs.length / 2)
      sort(less)(xs.filter(less(_,pivot))) :::
      xs.filter(_ == pivot) :::
      sort(less)(xs.filter(less(pivot,_)))
    }
  }
}