sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def depth[A](t: Tree[A]): Int = depth(t, 0)

  def depth[A](t: Tree[A], current_depth: Int): Int = t match {
    case Leaf(_) => current_depth + 1
    case Branch(left, right) => depth(left, current_depth + 1).max(depth(left, current_depth + 1))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match{
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A], memo: B)(f: (A, B) => B): B = t match{
    case Leaf(value) => f(value, memo)
    case Branch(left, right) => fold(right, fold(left, memo)(f))(f)
  }

  def size1[A](t: Tree[A]): Int = fold(t, 0)((_,b) => b + 1)
  def maximum1(t: Tree[Int]): Int = fold(t, -1)((a, memo) => a.max(memo))
}
