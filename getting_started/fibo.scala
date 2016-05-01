// Fibonaci tail recurrsion
object Fibo{
  def main(args: Array[String]): Unit = {
    println(fibo(10))
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibo_tail(n_1: Int, n_2: Int, current: Int, max: Int): Int = {
      if (max == 1 || max == 2)
        1
      else if (current == max)
        n_1 + n_2
      else
        fibo_tail(n_1 + n_2, n_1, current + 1, max)
    }
    fibo_tail(1, 1, 3, n)
  }
}
