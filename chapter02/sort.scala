object SortModule{
  // Usage
  // SortModule.isSorted(Array(1,2,3), (a: Int, b: Int) => a > b)
  def isSorted[A](a: Array[A], compare: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def checkSorted(max: Int, current: Int, a: Array[A]): Boolean = {
      if (current == max)
        true
      else if (compare(a(current), a(current - 1)))
        checkSorted(max, current + 1, a)
      else
        false
    }
    checkSorted(a.length, 1, a)
  }
}
