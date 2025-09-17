// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

// Note: we are using our own lazy lists, not the standard library

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h,t) => Some(h())

  def tail: LazyList[A] = this match
    case Empty => Empty
    case Cons(h,t) => t()

  /* Note 1. f can return without forcing the tail
   *
   * Note 2. this is not tail recursive (stack-safe) It uses a lot of stack if
   * f requires to go deeply into the lazy list. So folds sometimes may be less
   * useful than in the strict case
   *
   * Note 3. We added the type C to the signature. This allows to start with a
   * seed that is a subtype of what the folded operator returns.
   * This helps the type checker to infer types when the seed is a subtype, for 
   * instance, when we construct a list:
   *
   * o.foldRight (Nil) ((a,z) => a:: z)
   *
   * The above works with this generalized trick. Without the C generalization
   * the compiler infers B to be List[Nothing] (the type of Nil) and reports
   * a conflict with the operator.  Then we need to help it like that:
   *
   * o.foldRight[List[Int]] (Nil) ((a,z) => a:: z)
   *
   * With the C type trick, this is not neccessary. As it hints the type
   * checker to search for generalizations of B.
   *
   * I kept the foldLeft type below in a classic design, so that you can
   * appreciate the difference. Of course, the same trick could've been
   * applied to foldLeft.
   */
  def foldRight[B, C >: B](z: => B)(f: (A, => C) => C): C = this match
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))

  /* Note 1. Eager; cannot be used to work with infinite lazy lists. So
   * foldRight is more useful with lazy lists (somewhat opposite to strict lists)
   * Note 2. Even if f does not force z, foldLeft will continue to recurse.
   */
  def foldLeft[B](z: => B)(f :(A, => B) => B): B = this match
    case Empty => z
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)

  // Note: Do you know why we can implement find with filter for lazy lists but
  // would not do that for regular lists?
  def find(p: A => Boolean) = 
    this.filter(p).headOption

  // Exercise 2

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  

  // Test in the REPL, for instance: LazyList(1,2,3).toList 
  // (and see what list is constructed)

  // Exercise 3

  def take(n: Int): LazyList[A] =
    if n <= 0 then Empty
    else this match
      case Empty => Empty
      case Cons(h, t) => cons(h(), t().take(n-1))
    

  def drop(n: Int): LazyList[A] =
     if n <= 0 then this
     else this match
      case Empty => Empty
      case Cons(h, t) => t().drop(n-1)
     
  // naturals.take(1000000000).drop(41).take(10).toList
  // This terminates without exception, because of the lazy structure/laziness. take(1 bil) makes a billion elements available
  // when we need them/actually use them. drop(41) then skips the first 41 elements of the billion and keeps the rest, 
  // and take(10) then takes 10 elements and discards the rest. Only when toList is called do we actually force an evaluation
  // so when building this list we only compute the values used, i.e the 41 we drop and the 10 we take.
  // If the toList is instead moved to be after either drop(41) or take(1 bil), then we will be computing either a billion elements 
  // or a billion - 41 elements, resulting in an error. 
    

  // Exercise 4

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(_,_) => Empty

  // naturals.takeWhile { _ < 1000000000 }.drop(100).take(50).toList
  // The above also terminates quickly, by the same logic as in exercise 3. When toList is called and forces computations,
  // we only compute the 100 values we drop and the 50 we take.
      

  // Exercise 5
  
  def forAll(p: A => Boolean): Boolean = this match
    case Empty => true
    case Cons(h, t) if p(h()) => t().forAll(p)
    case Cons(h,_) => p(h())

  // Note 1. lazy; tail is never forced if satisfying element found this is
  // because || is non-strict
  // Note 2. this is also tail recursive (because of the special semantics
  // of ||)
  def exists(p: A => Boolean): Boolean = this match
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false

  // We use the following test case for forAll: naturals.forAll { _ < 0 }
  // If we used this one, it would be crashing: naturals.forAll { _ >=0 }. Why?
  // Recall that exists has already been implemented before (in the book). Both forAll and exists
  // are a bit strange for infinite lazy lists; you should not use them unless you know the result; but once
  // you know the result there is no need to use them. They are fine to use on finite lazy lists. Why?
  // The test case naturals.forAll { _ >=0 } would fail because it would not terminate, i.e all elements in the naturals list
  // fulfill the predicate of being bigger or equal to 0, which means we would be evaluating an infinite amount of elements.
  // Therefore using forAll and exists on infinite lists, if you don't know the answer, can result in an infinite amount of evaluations
  // which in the end would probably cause the program to crash. On a finite lazy list, this is fine, because the program will
  // eventually terminate, at the latest at the end of the list.
  
  

  // Exercise 6
  
  def takeWhile1(p: A => Boolean): LazyList[A] =
    foldRight(Empty)((h, t) => if p(h) then cons(h, t) else Empty)
  

  // Exercise 7
  
  def headOption1: Option[A] = 
    foldRight(None)((h, t) => Some(h))

  // Exercise 8
  
  // Note: The type is incorrect, you need to fix it
  def map[B](f: A => B): LazyList[B] = 
    foldRight(Empty)((h, t) => cons(f(h), t))

  // Note: The type is incorrect, you need to fix it
  def filter(p: A => Boolean): LazyList[A] = 
    foldRight(Empty)((h, t) => if p(h) then cons(h, t) else t)

  /* Note: The type is given correctly for append, because it is more complex.
   * Try to understand the type. The contsraint 'B >: A' requires that B is a
   * supertype of A. The signature of append allows to concatenate a list of
   * supertype elements, and creates a list of supertype elements.  We could have
   * written just the following:
   *
   * def append(that: => LazyList[A]): LazyList[A]
   *
   * but this would not allow adding a list of doubles to a list of integers
   * (creating a list of numbers).  Compare this with the definition of
   * getOrElse last week, and the type of foldRight this week.
   */
  def append[B >: A](that: => LazyList[B]): LazyList[B] = 
    foldRight(that)((h, t) => cons(h, t))

  // Note: The type is incorrect, you need to fix it
  // Is it alright to use append here or was something else intended?
  def flatMap[B](f: A => LazyList[B]): LazyList[B] = 
    foldRight(Empty)((h, t) => f(h).append(t))

  // Exercise 9
  // Type answer here
  //
  // ...
  //
  // Scroll down to Exercise 10 in the companion object below

  // Exercise 13

  def mapUnfold[B](f: A => B): LazyList[B] =
    ???

  def takeUnfold(n: Int): LazyList[A] =
    ???

  def takeWhileUnfold(p: A => Boolean): LazyList[A] =
    ???

  def zipWith[B >: A, C](ope: (=> B, => B) => C)(bs: LazyList[B]): LazyList[C] =
    ???

end LazyList // enum ADT



// The companion object for lazy lists ('static methods')

object LazyList:

  def empty[A]: LazyList[A] = Empty

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty 
    then empty
    else cons(as.head, apply(as.tail*))

  // Exercise 1

  def from(n: Int): LazyList[Int] =
    cons(n, from(n+1))

  def to(n: Int): LazyList[Int] =
    cons(n, to(n-1))

  lazy val naturals: LazyList[Int] =
    from(1)

  // Scroll up to Exercise 2 to the enum LazyList definition 
  
  // Exercise 10

  // Note: The type is incorrect, you need to fix it
  lazy val fibs: LazyList[Int] = 
    def go(curr: Int, next: Int): LazyList[Int] = 
      cons(curr, go(next, curr+next))
    go(0, 1)

  // Exercise 11

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    ???

  // Exercise 12

  // Note: The type is incorrect, you need to fix it
  lazy val fibsUnfold: Any = ???

  // Scroll up for Exercise 13 to the enum

end LazyList // companion object
