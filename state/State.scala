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

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nextRNG) = ra(rng)
      f(a)(nextRNG)
    }

  // map using flatMap
  def map3[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map4[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => 
      map(rb)(b => 
          f(a, b)))

  def nonNegativeEven: Rand[Int] = map(nonNegativeRNG)(i => i - i % 2)

  def nonNegativeRNG(rng: RNG): (Int, RNG) = {
    val (num, nextRNG) = rng.nextInt
    val positiveNum = if (num == Int.MinValue) -(num + 1)
    else if (num < 0) - num
    else num
    (positiveNum, nextRNG)
  }

  def nonNegativeInt: Rand[Int] = { rng => nonNegativeRNG(rng) }

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

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeRNG(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else
      nonNegativeLessThan(n)(rng2)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){i =>
      val m = i % n
      if (i + (n - 1) - m >= 0) unit(m) else nonNegativeLessThan2(n)
    }
}

case class State[S, +A](run: S => (A, S)){
  def unit[A](a: A): State[S, A] = State(s => (a, s))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, nextState) = run(s)
    f(a).run(s)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](b: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(
    a => b.map(b => f(a, b))
  )
}

object State{
  def sequence[S, A](lst: List[State[S, A]]): State[S, List[A]] = {
    def go(lst: List[State[S, A]], result: List[A], currentState: S): (List[A], S) = lst match {
      case Nil =>
        (result, currentState)
      case hd :: tl =>
        val (a, nextState) = hd.run(currentState)
        go(tl, a::result, nextState)
    }
    State((s:S) => go(lst, List(), s))
  }
  // use foldLeft
  // def sequence2[S, A](lst: List[State[S, A]]): State[S, List[A]] = State((s: State[S, A]) =>
  //     lst.foldRight((List(), s))((st, memo) => {
  //       val currentState = memo._2
  //       val currentLst = memo._1
  //       val (a, nextState) = st.run(currentState)
  //       (a::currentLst, nextState)
  //     }
  //     )
  //   )
}
