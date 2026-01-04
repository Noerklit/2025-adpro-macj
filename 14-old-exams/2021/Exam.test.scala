package adpro

import org.scalacheck.{Gen, Arbitrary, Prop}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean}

import fpinscala.answers.laziness.LazyList
import fpinscala.answers.state.*

object Exam2020AutumnSpec
  extends org.scalacheck.Properties("exam-2021"):

  // Q1: Warm-up
  property("Q1: hello returns correct strings for ADT") =
    Q1.hello(Q1.Printable.Triangle) == "triangle" && 
    Q1.hello(Q1.Printable.Square) == "square"


  // Q2: Either Sequence
  property("Q2.01: sequence handles all Rights") =
    forAll { (l: List[Int]) =>
      val input = l.map(Right(_))
      Q2.sequence(input) == Right(l)
    }

  property("Q2.02: sequence returns the LAST error (Left)") =
    val input: List[Either[String, Int]] = 
      List(Left("first"), Right(1), Left("last"), Right(2))
    Q2.sequence(input) == Left("last")


  // Q4: Triple Generator
  property("Q4: riid triple satisfies a <= x <= b") =
    forAll { (seed: Long) =>
      val rng = RNG.Simple(seed)
      val ((a, b, x), _) = Q4.riid.run(rng)
      val (lower, upper) = (Math.min(a, b), Math.max(a, b))
      x >= lower && x <= upper
    }


  // Q8: Efficient LazyList size check
  property("Q8: checkIfLongerEqThan is correct and efficient") =
    val hugeList = LazyList.from(1) // Infinite list
    // This should terminate instantly if implemented correctly
    Q8.checkIfLongerEqThan(hugeList)(10) == true &&
    Q8.checkIfLongerEqThan(LazyList(1, 2, 3))(5) == false


  // Q9: Property test for s.append(s)
  // We include your Q9 spec logic here as a property
  property("Q9: nonEmpty stream concatenated with itself is >= 2") =
    // Using a simple generator for non-empty lists
    forAll(Gen.nonEmptyListOf(Gen.choose(1, 100))) { (l: List[Int]) =>
      val s = LazyList(l*)
      Q8.checkIfLongerEqThan(s.append(s))(2)
    }


// Q10: Parallel flattening
  property("Q10: flatten Option[Par] to Par[Option]") =
    import fpinscala.answers.parallelism.Par
    import java.util.concurrent.*
    val es = Executors.newFixedThreadPool(2)
    
    val noneFlat = Q10.flatten(None)
    val someFlat = Q10.flatten(Some(Par.unit(42)))
    
    // Attempting the alternative parameter order:
    // Some versions use Par.run(par)(es), others use Par.run(es)(par)
    // Based on your error, it seems to want the Par first.
    Par.run(noneFlat)(es).get == None && 
    Par.run(someFlat)(es).get == Some(42)


  // Q12 & Q13: Monadic Loop
  property("Q12: sum using loop and Id monad is correct") =
    forAll { (l: List[Int]) =>
      // Only testing small-ish lists to avoid stack overflow in unoptimized loops
      l.size < 1000 ==> (Q12.sum(l) == l.sum)
    }

  property("Q13: loop identity property") =
    import Q12.idMonad
    // If predicate is false immediately, return initial
    val result = Q13.loop[Int, Q12.Id](10)(_ + 1)(_ < 10)
    result == 10

end Exam2020AutumnSpec