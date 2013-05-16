package ae.mohd874.algorithms.sort

import scala.collection.mutable.ListBuffer

object BucketSort {

  def apply[T](less: (T, T) => Boolean)(xs: List[T], buckets: List[T]) = {

  }

  def bucketSort[T](bucketSelector: (T) => Int)(xs: List[T], buckets: List[ListBuffer[T]]): Unit = {
    xs match {
      case Nil => List()
      case x :: tail => {
        val b = bucketSelector(x)
        buckets(b).append(x)
        bucketSort(bucketSelector)(tail, buckets)
      }
    }
  }
}