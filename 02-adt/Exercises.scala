// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List: 

  def head[A] (l: List[A]): A = l match
    case Nil => throw NoSuchElementException() 
    case Cons(h, _) => h                                                                                                                                                                                                                                       
  
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A] (l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2)) 

  def foldRight[A, B] (l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(a, as) => f(a, foldRight(as, z, f))
    
  def map[A, B] (l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]] (l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming)
  // import adpro.adt.List.*
  // List(1, 2, 3, 4, 5) match
  // case Cons(x, Cons(2, Cons(4, _))) => x
  // case Nil => 42
  // case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  // case Cons(h, t) => h
  // case _ => 101

  // I think the value of the match expression is 3, because we hit the 3'rd case, so x=1, y=2, x+y=3
  // The function called is the apply function

  // Exercise 2

  def tail[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(_, t) => t

  // Exercise 3
  
  def drop[A] (l: List[A], n: Int): List[A] = (l, n) match
    case (l, n) if n <= 0 => l
    case (Nil, n) => throw NoSuchElementException()
    case (Cons(_,t), n) => drop(t, n-1)
  

  // Exercise 4

  def dropWhile[A] (l: List[A], p: A => Boolean): List[A] = (l, p) match
    case (Nil, p) => Nil
    case (Cons(h,t),p) if p(h) => dropWhile(t, p)
    case (_,_) => l
  

  // Exercise 5
 
  def init[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  // I don't think this takes constant time or space as tail does, as we have to copy all the previous Cons objects
  // each time?
  

  // Exercise 6
  // The underscore is the element taken from the list, but this can be ignored, as we don't care about
  // its value, we simply want to increment the accumulator for each element.

  def length[A] (l: List[A]): Int = 
    foldRight(l, 0, (_, acc) => acc + 1)


  // Exercise 7
  @annotation.tailrec
  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = l match
    case Nil => z
    case Cons(a, as) => foldLeft(as, f(z, a), f)

    // Example of how to potentially apply some print statements to help debug (this would replace second case in foldLeft):
    // Cons(a, as) =>
    // println(s"apply f to (h=$z, a=$a)")
    // val z1 = f(z, a)
    // println(s" -> result $z1")
    // foldLeft(as, z1, f)
  
    
  // Exercise 8

  def product (as: List[Int]): Int = as match
    case Nil => 1
    case Cons(x, xs) => foldLeft(xs, x, (h, acc) => h * acc)


  def length1[A] (as: List[A]): Int =
    foldLeft(as, 0, (h, _) => h + 1)

  // Exercise 9

  def reverse[A] (l: List[A]): List[A] =
    foldLeft(l, Nil, (h, acc) => Cons(acc, h))

  // Is it possible with foldRight?
  // def reverse[A] (l: List[A]): List[A] =
  //   foldRight(l, Nil, (h, t) => Cons(h, (t, Nil)))

 
  // Exercise 10 (Not done)

  def foldRight1[A, B] (l: List[A], z: B, f: (A, B) => B): B =
    foldLeft(reverse(l), z, (h, acc) => f(acc, h))

  // Exercise 11

  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B = ???
 
  // Exercise 12
  // This uses explicit recursion, so should be rewritten with a HOF
  def concat[A] (l: List[List[A]]): List[A] = l match
    case Nil => Nil 
    case Cons(h, Nil) => h
    case Cons(h, acc) => append(h, concat(acc))

  
  // Exercise 13

  def filter[A] (l: List[A], p: A => Boolean): List[A] = ???
  

 
  // Exercise 14

  def flatMap[A,B] (l: List[A], f: A => List[B]): List[B] = ???

  // Exercise 15

  def filter1[A] (l: List[A], p: A => Boolean): List[A] = ???

  // Exercise 16

  def addPairwise (l: List[Int], r: List[Int]): List[Int] = ???

  // Exercise 17

  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = ???

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ???
