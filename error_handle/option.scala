package fpinscala.errorhandling
import scala.{Option => _, Some => _, Either => _, _}

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
}
