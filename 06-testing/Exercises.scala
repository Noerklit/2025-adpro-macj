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

  // Fails for Lazylist01 because in headoption implementation, if case Empty is not implemented, should return None
  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  // Fails for Lazylist02 because in the def cons, empty is returned instead of Cons(() => head, () => tail)
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
    ???

    
  // Exercise 5
  // This currently passes for lazylist02, because both sides simply produce an empty list, unsure if it should
  // I believe it should pass for lazylist01, as the headOption issue is not relevant here.

  property("Ex05.1: l.take(n).take(n) == l.take(n) for any lazy list s and any n.") =
    
    // Here I only need to define my generator for a nonEmptyLazyList, since ScalaCheck has one to generate a random integer it just uses
    // to make the n, in the next exercise I have to define my own since my integers can't be negative.
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (s: LazyList[Int], n: Int) => s.take(n).take(n).toList == s.take(n).toList}
  
  // Exercise 6
  // Have to pass in the arguments to forAll to ensure that the values of n and m are valid values (positive i)
  property("Ex06.1: l.drop(n).drop(m) == l.drop(n+m) for any n, m.") =
    
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    val nGen = Gen.choose(0, 1000)
    val mGen = Gen.choose(0, 1000)
    forAll(genNonEmptyLazyList, nGen, mGen) {(l, n, m) => l.drop(n).drop(m).toList == l.drop(n+m).toList}
  
  // Exercise 7
  property("Ex07.1: l.drop(n) does not force any of the dropped elements (heads). Should hold even if some element in tail is forced") =
    ???

  // Exercise 8
  property("Ex08.1: l.map(identity) == l for any lazy list l. Here identity is the identity function.") =
    ???

  // Exercise 9
  property("Ex09.1: map terminates on infinite lazy lists.") =
    ???
 
  // Exercise 10
  property("Ex10.1: ") =
    ???

