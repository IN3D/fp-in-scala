package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil        => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(x,xs)   => x * product(xs)
  }

  // Variadic function syntax
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil        => Nil
    case Cons(_, t) => t
  }

  // Exercise 3
  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil        => Nil
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 4
  def drop[A](as: List[A], n: Int): List[A] =
    if (n <= 0) as
    else as match {
      case Nil        => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  // Exercise 5
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _          => as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  // Exercise 6
  def init[A](as: List[A]): List[A] = as match {
    case Nil         => Nil
    case Cons(_,Nil) => Nil
    case Cons(h,t)   => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // Exercise 9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil       => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }

  // Exercise 11.a
  def sumLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)
  // Exercise 11.b, I think this will fail first
  def productLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)
  // Exercise 11.c
  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  // Exercise 12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((xs,h) => Cons(h,xs))

  // Exercise 13
  def foldRightOptimized[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a,b))

  // Exercise 14
  def appendOptimized[A](l: List[A], r: List[A]): List[A] =
    foldRightOptimized(l, r)(Cons(_,_))

  // Exercise 15
  // foldRightOptimized(lss, List[A]())((acc, ls) => appendOptimized(acc, ls))
  def concat[A](lss: List[List[A]]): List[A] =
    foldRightOptimized(lss, List[A]())(appendOptimized)
}
