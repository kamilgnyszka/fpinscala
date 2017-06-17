package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
def sum(ints: List[Int]): Int =  // A function that uses pattern matching to add up a list of integers
   // The sum of the empty list is 0.
    foldLeft(ints,0)(_+_) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.


  def product(ds: List[Double]): Double =
    foldLeft(ds,1.0)(_*_)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def appendFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((h,acc) => Cons(h,acc))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => Cons(h,xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => if (n == 0) l else drop(xs,n-1)
  }

  def dropWhile[A](l: List[A])( f: A => Boolean): List[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x,init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l,0) ((x,y) => (1 + y))

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs,(f(z,x)))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def add1(l: List[Int]) : List[Int] =
    foldRight(l,Nil:List[Int])((x,xs) => Cons(x+1,xs))

  def dToString(l: List[Double]) : List[String] =
    foldRight(l,Nil:List[String]) ((x,xs) => (Cons(x.toString,xs)))

    def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l,Nil:List[B])((x,xs) => Cons(f(x),xs))

  def filter[A,B](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l,Nil:List[A])((x,xs) => if(f(x)) Cons(x,xs) else xs )
}
