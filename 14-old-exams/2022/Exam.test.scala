package adpro

import org.scalacheck.{Gen, Arbitrary, Prop}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}

import ExceptionalOptions.*
import PrimesAndLaziness.*
import ApplesToApples.*
import SizedLists.*

object Exam2022AutumnSpec
  extends org.scalacheck.Properties("exam-2022"):

  // Q1 & Q2: ExceptionalOptions

  property("Q01: SafeTotal handles exceptions and returns Option") = 
    val f = (s: String) => s.toInt
    val safeF = SafeTotal(f)
    (safeF("42") == Some(42)) && (safeF("not a number") == None)

  property("Q02: headOption works correctly via SafeTotal") =
    (headOption(List(1, 2, 3)) == Some(1)) && (headOption(Nil) == None)


  // Q3: PrimesAndLaziness

  property("Q03.01: primesApart(2) generates correct twin primes") =
    val twins = primesApart(2).take(3).toList
    twins == List((3, 5), (5, 7), (11, 13))

  property("Q03.02: p1, p2 and p3, p4 are correctly found for distance 10") =
    (p2 - p1 == 10) && (p4 - p3 == 10) && (p1 != p3)


  // Q5: Implementation of the required primesApart property test

  property("Q05: Elements in pairs returned by primesApart differ by n") = 
    // We choose even n between 2 and 20 as requested
    forAll(Gen.choose(1, 10).map(_ * 2)) { (n: Int) =>
      val pairs = primesApart(n).take(5).toList
      pairs.forall { case (first, second) => second - first == n }
    }


  // Q6 & Q7: ApplesToApples

/*   property("Q06/Q07: pickBetter and betterThan work with Apple type class") =
    val a1 = Apple(500)
    val a2 = Apple(200)
    // Testing the type class instance and the extension method
    (pickBetter(a1, a2) == a1) && (a1 betterThan a2) */


// Q8, Q9, Q10: SizedLists

/*   property("Q08: l2 is correctly typed and contains three elements") =
    // If tail works 3 times to reach Empty, the type-level size is exactly 3
    tail(tail(tail(l2))) == SizedList.Empty

  property("Q09: third element is accessible and correct") =
    // l2 should be Cons(3, Cons(1, Cons(4, Empty)))
    // This test only compiles if your 'third' signature is correct!
    third(l2) == 4

  property("Q10: append increases the type-level size and maintains order") =
    val l_short = SizedList.Cons(10, SizedList.Empty)
    val l_longer = append(20, l_short)
    // After appending to a size 1 list, the second element should be our value
    // This checks both the value and the recursive structure
    head(tail(l_longer)) == 20 */

end Exam2022AutumnSpec