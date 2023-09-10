package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

//  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
//    as match
//      case Nil => acc
//      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (acc1, x: A) => f(x, acc1))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("tail of an empty list")
    case Cons(x, xs) => xs

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => sys.error("there is not head of an empty list")
      case Cons(_, tl) => Cons(h, tl)

  def drop[A](l: List[A], n: Int): List[A] = {
    @scala.annotation.tailrec
    def loop(left: List[A], i: Int): List[A] =
      left match
        case Nil => Nil
        case other => if i == n then left else loop(tail(other), i + 1)

    if n < 0 then l else loop(l, 0)
  }

  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case a @ Cons(hd, tl) => if f(hd) then dropWhile(tl, f) else a
      case other => other

//  def init[A](l: List[A]): List[A] =
//    @scala.annotation.tailrec
//    def loop(accu: List[A], leftover: List[A]): List[A] =
//      leftover match
//        case Nil => sys.error("init of an empty list")
//        case Cons(hd, Nil) => accu
//        case Cons(hd, tl) => loop(append(accu, Cons(hd, Nil)), tail(leftover))
//    loop(Nil, l)

  def init[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("init of an empty list")
      case Cons(h, Nil) => Nil
      case Cons(h, tl) => Cons(h, init(tl))

  def length[A](l: List[A]): Int = foldRight(l, 0, (_, b) => b + 1)

  @scala.annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(h, tl) => foldLeft(tl, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft[Double, Double](ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (b, _) => b + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A], (acc, hd) => Cons(hd, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (x, acc) => Cons(x, acc))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A], (acc, list) => appendViaFoldRight(acc, list))

  def incrementEach(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int], (x, acc) => Cons(x + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String], (x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A], f: A => B): List[B] = foldRight[A, List[B]](l, Nil: List[B], (x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] = foldRight[A, List[A]](as, Nil: List[A], (x, acc) =>
      if f(x) then Cons(x, acc) else acc
    )

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = flatMap(as, x =>
      if f(x) then Cons(x, Nil) else Nil: List[A]
    )

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Cons(h1, tl1), Cons(h2, tl2)) => Cons(h1 + h2, addPairwise(tl1, tl2))
//      case (Cons(h1, tl1), Nil) => Cons(h1, addPairwise(tl1, Nil))
      case (_, Nil) => Nil
//      case (Nil, Cons(h2, tl2)) => Cons(h2, addPairwise(Nil, tl2))
      case (Nil, _) => Nil

  def zipWith[A, B](a: List[A], b: List[A])(op: (A, A) => B): List[B] =
     (a, b) match
       case (Cons(h1, tl1), Cons(h2, tl2)) => Cons(op(h1,h2), zipWith(tl1, tl2)(op))
       case (_, Nil) => Nil
       case (Nil, _) => Nil

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    @scala.annotation.tailrec
    def loop(sup1: List[A], sub1: List[A]): Boolean =
      (sup1, sub1) match
        case (_, Nil) => true
        case (Cons(a, tl1), Cons(b, tl2)) if a == b => loop(tl1, tl2)
        case (_, _) => false

    sub match
      case Cons(h, tl) => dropWhile(sup, x => x != h) match
        case Nil => false
        case nonempty => loop(nonempty, sub)
      case _ => true

