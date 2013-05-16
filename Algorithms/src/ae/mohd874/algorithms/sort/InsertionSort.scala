package ae.mohd874.algorithms.sort

object InsertionSort {

  def apply[T](less: (T, T) => Boolean)(xs: List[T]) = {
    sort(less)(xs)
  }

  def sort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {

    def sortInsert[T](less: (T, T) => Boolean)(xs: List[T], acc: List[T]): List[T] = xs match {
      case x :: Nil => insert(less)(acc, x)
      case x :: tail => {
        sortInsert(less)(tail, insert(less)(acc, x))
      }
    }

    sortInsert(less)(xs, List[T]())
  }

  def insert[T](less: (T, T) => Boolean)(xs: List[T], i: T): List[T] = {
    def accmulatedInsert[T](less: (T, T) => Boolean)(xs: List[T], i: T, acc: List[T]): List[T] = {
      xs match {
        case Nil => acc ::: i :: List()
        case x :: tail if (less(x, i)) => {
          val res: List[T] = acc ::: List(x)
          accmulatedInsert(less)(tail, i, res)
        }
        case x :: tail => acc ::: i :: x :: tail
      }
    }

    accmulatedInsert(less)(xs, i, List())
  }

}