trait RNG{
  def nextInt: (Int, RNG)
}

object RNG{
  case class Simple(seed: Long) extends RNG{
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeRNG)(i => i - i % 2)

  def nonNegativeRNG(rng: RNG): (Int, RNG) = {
    val (num, nextRNG) = rng.nextInt
    val positiveNum = if (num == Int.MinValue) -(num + 1)
    else if (num < 0) - num
    else num
    (positiveNum, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, nextRNG) = nonNegativeRNG(rng)
    val doubleNum = num.toDouble / Int.MaxValue
    (doubleNum, nextRNG)
  }

  def double2: Rand[Double] = map(nonNegativeRNG)(i => i.toDouble / Int.MaxValue)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intNum, nextRNG1) = rng.nextInt
    val (doubleNum, nextRNG2) = double(nextRNG1)
    ((intNum, doubleNum), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (intNum, nextRNG1) = rng.nextInt
    val (doubleNum, nextRNG2) = double(nextRNG1)
    ((doubleNum, intNum), nextRNG2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doubleNum1, nextRNG1) = double(rng)
    val (doubleNum2, nextRNG2) = double(nextRNG1)
    val (doubleNum3, nextRNG3) = double(nextRNG2)
    ((doubleNum1, doubleNum2, doubleNum3), nextRNG3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int, lst: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count == 0) (lst, rng)
      else{
        val (num, nextRNG) = rng.nextInt
        go(count - 1, num::lst)(nextRNG)
      }
    }
    go(count, List())(rng)
  }
}
