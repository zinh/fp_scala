// val intList = Gen.listOf(Gen.choose(0, 100))
// val prop = 
//   forAll(intList)(ns => ns.reverse.reverse == ns) &&
//   forAll(intList(ns => ns.headOption == ns.reverse.lastOption)
case class Gen[A](sample: State[RNG, A])
object Gen{
  def listOf[A](a: Gen[A]): Gen[List[A]]
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
  def choose(start: Int, stopExclusive: Int): Gen[Int]
  def unit[A](a: => A): Gen[A]
  def boolean: Gen[Boolean]
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]]
}

object Prop{
  type SuccessCount = Int
  type FailedCase = String
}

trait Prop {
  def &&(p: Prop): Prop = this.check && p.check
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}
