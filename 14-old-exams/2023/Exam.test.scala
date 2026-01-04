/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}

import org.scalacheck.*
import org.scalacheck.Prop.*
import Arbitrary.arbitrary

import adpro.laziness.LazyList
import adpro.Streaming.*
import adpro.Parsing.*
import adpro.Game.*
import adpro.RL.*

object ExamSpec
  extends org.scalacheck.Properties("exam-2023-autumn"):

  property("A test that always passes (a sanity check)") = 
    forAll { (n: Int) => n == n }

    // --- QUESTION 1 (Streaming) ---

  property("Q01.01: fViaFold is equivalent to fViaRec") = 
    forAll { (l: List[Int]) =>
      val ll = LazyList(l*)
      fViaFold(ll) == fViaRec(ll)
    }

  property("Q01.02: fViaFold handles empty streams") =
    fViaFold(LazyList.Empty) == 0

  // --- QUESTION 2 & 3 (Parsing) ---

  import adpro.parsing.Sliceable.run

  property("Q02.01: longestLine returns correct max columns") =
    val input = "1,2,3\n4,5,6,7,8\n9,10"
    longestLine.run(input) == Right(5)

  property("Q03.01: allLinesTheSame returns true for uniform lines") =
    val input = "1,2,3\n4,5,6\n7,8,9"
    allLinesTheSame.run(input) == Right(true)

  property("Q03.02: allLinesTheSame returns false for non-uniform lines") =
    val input = "1,2,3\n4,5\n7,8,9"
    allLinesTheSame.run(input) == Right(false)

  // --- QUESTION 4, 5, 6 (Game) ---

  property("Q05.01: game result probability sum is 1.0") =
    // Note: This requires a given RNG in scope to sample
    import Game.given
    val outcomes = game(Alice, Bob).sample(1000)
    // Pigaro distributions should sum to 1.0 approximately
    val totalPr = outcomes.pr(Some(Player.P1)) + 
                  outcomes.pr(Some(Player.P2)) + 
                  outcomes.pr(None)
    totalPr >= 0.99 && totalPr <= 1.01

  // --- QUESTION 7 & 8 (RL) ---

  // We can reuse your NullUpdatesSpec logic here
  property("Q07.01: Null update on zero table returns identical table") =
    val q = qZero(2, 3)
    update(q, 0, 0)(0.0, 0.0) == q

  property("Q08.01: Update with current value and 0 estimate is identity") =
    // Using a property test with random Q-tables
    forAll(qGen(2, 3)) { q =>
      forAll(Gen.choose(0, 1), Gen.choose(0, 2)) { (s, a) =>
        val currentReward = q(s)(a)
        update(q, s, a)(currentReward, 0.0) == q
      }
    }

  // --- QUESTION 9 (Lenses) ---

  property("Q09.01: updateWithLens is equivalent to update") =
    forAll(qGen(2, 2)) { q =>
      forAll(Gen.choose(0, 1), Gen.choose(0, 1), arbitrary[Double], arbitrary[Double]) { 
        (s, a, r, e) =>
          updateWithLens(q, s, a)(r, e) == update(q, s, a)(r, e)
      }
    }

end ExamSpec

object NullUpdatesSpecObj
  extends RL.NullUpdatesSpec(update = RL.update, "studentrl") {}




