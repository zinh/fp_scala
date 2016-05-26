import java.util.concurrent._

object Par{
  type Par[A] = ExecutorService => Future[A]
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es:ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A]{
      def call = a(es).get
    })

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sortPar(lst: Par[List[Int]]): Par[List[Int]] = map(lst)(_.sorted)

  def parMap[A, B](lst: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = lst.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](lst: List[Par[A]]): Par[List[A]] = lst.foldLeft(unit(List(): List[A]))(
    (memo: Par[List[A]], pa: Par[A]) => 
      map2(memo, pa)((lst: List[A], a: A) => (a::lst))
    )

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = as.foldRight(unit(List(): List[A]))((a, memo) =>
      map(memo)(lst => 
          if (f(a))
            a::lst
          else
            lst
            )
      )
}
