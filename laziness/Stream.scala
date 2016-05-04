// package fpinscala.laziness
import Stream._
trait Stream[+A]{
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(hd, tl) => hd()::(tl().toList)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(hd, tl) if n > 0 => cons(hd(), tl().take(n - 1))
    case _ => empty
  }

  def drop(n: Int):Stream[A] = this match {
    case Cons(hd, tl) if n > 0 => tl().drop(n - 1)
    case Cons(_, tl) if n == 0 => tl()
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    this match {
      case Cons(hd, tl) => f(hd(), tl().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((item, memo) => p(item) || memo)
  def forAll(p: A => Boolean): Boolean = foldRight(true)((item, memo) => p(item) && memo)
  // implement takeWhile using foldRight
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((hd, tl) => if (p(hd)) cons(hd, tl) else empty)
  // implement headOption using foldRight
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((hd, tl) => 
      cons(f(hd), tl))
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((hd, memo) => if (p(hd)) cons(hd, memo) else memo)
  def find(p: A => Boolean): Option[A] = headOption(filter(p))

  // Using unfold
  // def map2[B](f: A => B): Stream[B]
  // def take2(n: Int): Stream[A]
  // def takeWhile(p: A => Boolean): Stream[A]
  // def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C]
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream{
  def empty[A]: Stream[A] = Empty
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def headOption[A](a: Stream[A]): Option[A] = a match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((nextValue, nextState)) => cons(nextValue, unfold(nextState)(f))
  }

  def fibs2: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def from2(n: Int): Stream[Int] = unfold(n)(s => 
      Some(s, s + 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))
}
