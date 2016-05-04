// package fpinscala.errorhandling
// import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(value) if f(value) => Some(value)
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(lst: Seq[Double]): Option[Double] = {
    if (lst.isEmpty) None
    else Some(lst.sum / lst.length)
  }

  def variance(lst: Seq[Double]): Option[Double] = {
    mean(lst) flatMap(m => mean(lst.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] ={
    try Some(a)
    catch { case e: Exception => None }
  }
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap(aa => 
      b map (bb => 
          f(aa, bb)))

  def map3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldLeft(Some(List():List[A]))((memo, item) => memo.flatMap(lst => item map(i => i::lst)))
  // def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight(Some(List():List[B]))((aa, memo) => memo.flatMap(lst => f(aa).map(item => item::lst) ))
}
