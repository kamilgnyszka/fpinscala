package fpinscala.testing


import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}
import language.postfixOps
import language.implicitConversions


/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize,TestCases,RNG) => Result){
  def && (p: Prop): Prop = Prop {
    (m,n,rng) => run(m,n,rng) match {
      case Passed | Proved => p.run(m,n,rng)
      case x  => x
    }
  }

  def || (p: Prop): Prop = Prop {
    (m,n,rng) => run(m,n,rng) match {
      case Falsified(f,s) => p.run(m,n,rng) match {
        case Passed | Proved => Falsified(f,s)
        case Falsified(fp,sp) => Falsified(f + " " + fp, s + sp)
      }
      case x  => x
    }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result{
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }


  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }
}

case class Gen[+A](sample: State[RNG,A]){
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample).flatMap(f)
  }

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)


  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => this.listOfN(n))
  }

  def unsized: SGen[A] = SGen(_ => this)



}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.int).map((_ % 2  == 0)))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.int).map(x => x % (stopExclusive - start) + start))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean flatMap(x => if (x) g1 else g2)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(x => g.listOfN(x))

  val smallInt = Gen.choose(-10,10)
  val maxProp = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  val sortedProp = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a,b) => a > b }
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
}

