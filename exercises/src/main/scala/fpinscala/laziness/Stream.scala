package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(x,xs) => x() :: xs().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(x,xs) => if (n <=0) Empty else Stream.cons(x(),xs().take(n-1))
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => this
    case Cons(x,xs) => if (!p(x())) Empty else Stream.cons(x(),xs().takeWhile(p))
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a,b)=> if (p(a)) Stream.cons(a,b) else Stream.empty[A])

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a),b))

  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some(f(h()),t())
      case _  => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a,b) => if (p(a)) Stream.cons(a,b) else b)

  def append[B>:A](that: Stream[B]): Stream[B] =
    foldRight(that)((a,b) => Stream.cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = constant2(1)

  def constant[A](a: A) : Stream[A] =
    cons(a,constant(a))

  def from(n: Int): Stream[Int] =
  Stream.cons(n,from(n+1))

  private final def fib(a:Int,b:Int): Stream[Int] = cons(a,fib(b,a+b))

  def fibs: Stream[Int] = fib(0,1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty[A]
      case Some((a,b)) => cons(a,unfold(b)(f))
    }
  }

  def fibs2: Stream[Int] =
    unfold[Int,(Int,Int)]((0,1))(x => Some(x._1,(x._2,x._1+x._2)))

  def from2(n: Int): Stream[Int] =
    unfold(n)(x => Some(x,x+1))

  def constant2[A](a: A) : Stream[A] =
    unfold(a)(x => Some(x,x))
}
