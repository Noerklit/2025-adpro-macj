// Run with: scala-cli run . --main-class adpro.TestExam
// Set breakpoints in the Exam.scala file, then click debug above the object TestingExam to run with debug mode

package adpro

import Streaming.*
import Parsing.*
import Game.*
// This specific import is needed to bring the 'rng' into scope for .sample()
import Game.given 
// This import is needed to enable the .run extension method for parsers
import adpro.parsing.Sliceable.run

import adpro.laziness.LazyList
import org.scalacheck.Prop

object TestingExam extends App:
    
  // --- Test Question 1 (Streaming) ---
  println("--- Question 1: Streaming ---")
  val l = LazyList(1, 2, 3, 4, 5)
  val result1 = fViaFold(l)
  println(s"Odd count (expected 3): $result1") 

  // --- Test Question 2 & 3 (Parsing) ---
  println("\n--- Question 2 & 3: Parsing ---")
  val csvInput = "1,2,3 , 5,4\n42,42"
  
  // .run is now available thanks to the Sliceable.run import
  val parsed = Parsing.parser.run(csvInput)
  println(s"Parsed CSV: $parsed")
  
  val longest = Parsing.longestLine.run(csvInput)
  println(s"Longest line (expected Right(5)): $longest")
  
  val same = Parsing.allLinesTheSame.run(csvInput)
  println(s"All lines same (expected Right(false)): $same")

  // --- Test Question 4, 5, 6 (Game/Pigaro) ---
  println("\n--- Question 5 & 6: Game ---")
  println(s"Probability Alice wins: $aliceFraction")
  
  // .sample(1000) now finds the 'rng' given in Game
  val fullDist = game(Alice, Bob).sample(1000)
  println(s"Sample distribution Alice vs Bob: ${fullDist}")

  // --- Test Question 7, 8, 9 (RL & Lenses) ---
  println("\n--- Question 7 & 8: RL Property Tests ---")
  
  object MyRLSpec extends RL.NullUpdatesSpec(RL.update, "RLSpec")
  MyRLSpec.check() 

  println("\n--- Question 9: Lens Update ---")
  val q0 = RL.qZero(2, 2)
  val qUpdated = RL.updateWithLens(q0, 0, 1)(10.0, 0.0)
  println(s"Value at (0,1) after lens update: ${qUpdated(0)(1)}")

  println("\nTests completed.")