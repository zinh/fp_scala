package fpscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](lst: List[A], e: A): List[A] = {
    Cons(e, lst)
  }

  def drop[A](lst: List[A], size: Int): List[A] = {
    if (size == 0)
      lst
    else if (lst == Nil)
      Nil
    else {
      drop(tail(lst), size - 1)
    }
  }

  def dropWhile[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => lst
  }

  def select[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, select(xs)(f))
    case Cons(x, xs) if !f(x) => select(xs)(f)
  }

  def init[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) =>
      Cons(x, init(xs))
  }

  def foldRight[A, B](lst: List[A], z: B)(f: (A, B) => B): B = lst match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A, B](lst: List[A], z: B)(f: (B, A) => B): B = lst match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](lst: List[A]): List[A] = foldLeft(lst, Nil:List[A])((memo, a) => Cons(a, memo))

  def sum1(lst: List[Int]): Int = foldLeft(lst, 0)((memo, a) => memo + a)

  def product1(lst: List[Int]): Int = foldLeft(lst, 1)((memo, a)  => memo  * a)

  // @annotation.tailrec
  def map[A, B](lst: List[A])(f: A => B): List[B] = lst match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => filter(xs)(f)
    case Cons(x, xs) => Cons(x, filter(xs)(f))
  }

  // def flatMap[A, B](lst: List[A])(f: A => List[B]): List[B] = lst match {
  //   case Nil => Nil
  //   case Cons(x, xs)
  // }

  def zipWith[A, B, C](lst1: List[A], lst2: List[B])(f: (A, B) => C): List[C] = (lst1, lst2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def takeWhile[A](lst: List[A])(f: A => Boolean): List[A] = {takeWhile(lst, Nil)(f)}

  def takeWhile[A](lst: List[A], memo: List[A])(f: A => Boolean): List[A] = lst match {
    case Cons(x, xs) if f(x) => takeWhile(xs, Cons(x, memo))(f)
    case _ => memo
  }

  def forAll[A](lst: List[A])(f: A => Boolean): Boolean = lst match {
    case Nil => true 
    case Cons(x, xs) if f(x) => forAll(xs)(f)
    case _ => false
  }

  def exists[A](lst: List[A])(f: A => Boolean): Boolean = lst match {
    case Nil => false
    case Cons(x, xs) if f(x) => true
    case Cons(x, xs) => exists(xs)(f)
  }
}
