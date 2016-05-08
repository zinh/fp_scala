import java.util.concurrent._

object Par{
  type Par[A] = ExecutorService => Future[A]
  // Computation immediately results in value of a
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  // mark parallelism
  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) => es.submit(new Callable[A]{
      def call = a(es).get
    })
  // spawn and run
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)
  // combine result of two parallel computation
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    (es: ExecutorService) => {
      val af = pa(es)
      UnitFuture(f(af.get))
    }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => map(Unit(a))(f)
}
