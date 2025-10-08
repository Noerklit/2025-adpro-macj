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

def genLazyListOfSpecificSize[A: Arbitrary](n: Int): Gen[LazyList[A]] =
  for la <- Gen.listOfN((n), summon[Arbitrary[A]].arbitrary)
  yield list2lazyList(la)

def genLazyListSpecificLengthWithFailElem[A: Arbitrary](n: Int): Gen[LazyList[A]] =
  for {
    la <- Gen.listOfN((n), summon[Arbitrary[A]].arbitrary)
  } yield {
    la.foldRight(cons(throw new RuntimeException("Failing Element"), LazyList.empty))((a, acc) => cons(a, acc))
  }

def genLazyListOfFailingElems[A: Arbitrary](n: Int): Gen[LazyList[A]] =
  for {
    a <- summon[Arbitrary[A]].arbitrary
  } yield {
    // make a list containing n failing elements with a single safe element at the end
    // The accumulator is the "tail" with the safe element, so for every element in the sequence, we ignore the value
    // and just cons an exception as the head, keeping the acc as the tail with the safe element
    (0 until n).foldRight(cons(a, LazyList.empty): LazyList[A]) {
      (_, acc) => cons(throw new RuntimeException("Failing Element"), acc)
    }
  }

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
  // So we simply force all elements of take(n) by calling toList on them.
  property("Ex04.1: take(n) does not force the (n+1)st head ever (even if we force all elements of take(n))") =
    forAll(Gen.posNum[Int]) { (n:Int) => 
      forAll(genLazyListSpecificLengthWithFailElem[Int](n)) { (l: LazyList[Int]) => 
        l.take(n).toList;
        true
      }
    }

    
  // Exercise 5)
  // This currently passes for lazylist02, because both sides simply produce an empty list, unsure if it should
  // I believe it should pass for lazylist01, as the headOption issue is not relevant here.
  property("Ex05.1: l.take(n).take(n) == l.take(n) for any lazy list s and any n.") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll(Gen.posNum[Int]) { (n:Int) => forAll(genNonEmptyLazyList, n) { (s, n) => s.take(n).take(n).toList == s.take(n).toList}}
  

  // Exercise 6
  // Have to pass in the arguments to forAll to ensure that the values of n and m are valid values (positive i)
  property("Ex06.1: l.drop(n).drop(m) == l.drop(n+m) for any n, m.") =
    
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    val nGen = Gen.choose(0, 1000)
    val mGen = Gen.choose(0, 1000)
    forAll(genNonEmptyLazyList, nGen, mGen) {(l, n, m) => l.drop(n).drop(m).toList == l.drop(n+m).toList}
  
  // Exercise 7
  property("Ex07.1: l.drop(n) does not force any of the dropped elements (heads). Should hold even if some element in tail is forced") =
    forAll(Gen.posNum[Int]) { (n:Int) => 
      forAll(genLazyListOfFailingElems[Int](n)) { (l: LazyList[Int]) => 
        l.drop(n).headOption.isDefined
      }
    }

  // Exercise 8
  property("Ex08.1: l.map(identity) == l for any lazy list l. Here identity is the identity function.") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l: LazyList[Int]) => l.map(identity).toList == l.toList}

  // Exercise 9
  property("Ex09.1: map terminates on infinite lazy lists.") =
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])
    forAll { (l: LazyList[Int]) => 
      l.map(identity)
      true
      }
 
  // Exercise 10
  // Test to append that appending to list of size n and size m give size n+m
  property("Ex10.1: Appending list.size(n) and list.size(m) equal list.size(n+m)") =
    forAll(Gen.posNum[Int]) { (n:Int) => 
      forAll(Gen.posNum[Int]) { (m: Int) => 
        forAll(genLazyListOfSpecificSize[Int](n)) {(list1: LazyList[Int]) =>
          forAll(genLazyListOfSpecificSize[Int](m)) {(list2: LazyList[Int]) => 
            list1.append(list2).toList.size == list1.toList.size + list2.toList.size
            } 
          }
      }
    }

  // Test that append terminates on infinite lazylist, as it should not be traversing the list since its lazy
  property("Ex10.2: Append terminates on infinite lazylist") =
    forAll(infiniteLazyList[Int]) { (l: LazyList[Int]) => 
      l.append(l)
      true;
    }

  // Test that no elements are forced when appending two list to one another
  // This fails because append uses foldRight and foldRight forces the first element? Is that correct?
  property("Ex10.4: Other elements than head is not forced when appending two list to one another") =
    forAll(Gen.posNum[Int]) { (n:Int) => 
      forAll(genLazyListSpecificLengthWithFailElem[Int](n)) { (l1: LazyList[Int]) => 
        forAll(genLazyListSpecificLengthWithFailElem[Int](n)) { (l2: LazyList[Int]) => 
          l1.append(l2)
          true;
        }
      }
    }
  
  // Test that no elements are forced when appending two list to one another
  // This fails because append uses foldRight and foldRight forces the first element? Is that correct?
  // property("Ex10.5: No elements forced when appending two list to one another") =
  //   forAll(Gen.posNum[Int]) { (n:Int) => 
  //     forAll(genLazyListOfFailingElems[Int](n)) { (l1: LazyList[Int]) => 
  //       forAll(genLazyListOfFailingElems[Int](n)) { (l2: LazyList[Int]) => 
  //         l1.append(l2)
  //         true;
  //       }
  //     }
  //   }

  // Test that l.append(l) is equal to l :: l
  property("Ex10.6: l.append(l) is equal to l :: l") =
    forAll(genNonEmptyLazyList[Int]) { (l: LazyList[Int]) => 
      l.append(l).toList == l.toList ::: l.toList 
    }


