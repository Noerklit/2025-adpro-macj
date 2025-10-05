// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
// import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] = 
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
  yield list2lazyList(la)

  
/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
  * not tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None => empty
  Gen.const(loop)

def genLazyListSpecificLength[A: Arbitrary](n: Int): Gen[LazyList[A]] =
  for la <- Gen.listOfN(n, arbitrary[A])
  yield list2lazyList(la)

/* The test suite */

object LazyListSpec 
  extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random" 

  // Exercise 2
  property("Ex02.1: headOption does not force the tail of a lazy list") = 
    val badTail: LazyList[Int] = LazyList.cons(throw new RuntimeException("tail forced"), LazyList.empty)
    LazyList.cons(42, badTail).headOption.contains(42)
  
  
  // Exercise 3
  property("Ex03.1: take does not force any heads nor any tails of the lazy list it manipulates") =
    val badTail: LazyList[Int] = LazyList.cons(throw new RuntimeException("tail forced"), LazyList.empty)
    val badHead = LazyList.cons(throw new RuntimeException("Head forced"), badTail)
    badHead.take(1) != null


  // Exercise 4
  property("Ex04.1: take(n) does not force the (n+1)st head ever (even if we force all elements of take(n))") =
    val n = 100
     forAll(genLazyListSpecificLength[Int](n)) { prefix =>
      val poisoned = LazyList.cons(
        throw new RuntimeException("poisoned!"),
        LazyList.empty
      )
      val list = prefix.append(poisoned)

      list.take(n).toList
      true
     }

    
  
  // Exercise 5
  
  // Exercise 6
  
  // Exercise 7

  // Exercise 8

  // Exercise 9
 
  // Exercise 10

