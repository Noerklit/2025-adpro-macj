/* Final Exam: Advanced Programming, by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2023: 05 January 2024
 *
 * The exam consists of 9 questions to be solved within 4 hours.
 * Solve the tasks in the file 'Exam.scala' (this file).
 *
 * You can use all functions provided in the included files,  as well
 * as functions we implemented in the course (if the source is missing
 * in this folder, you can add it to this file, so that things compile)
 *
 * You can access any static written aids, also online, but you are
 * not allowed to communicate with anybody or with anything (bots).
 * Using GitHub copilot ChatGPT and similar large language models
 * during the exam is not allowed. By submitting you legally declare to
 * have solved the problems alone, without communicating with anybody.
 *
 * Do not modify this file in other ways than answering the questions
 * (adding imports is allowed). Do not reorder the answers, and do not
 * remove question numbers or comments from the file. 
 *
 * Submit this file and only this file to LearnIT. Do not convert to
 * any other format than .scala. Do not submit the entire zip archive.
 * The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * The answers will be graded manually. We focus on the correctness of
 * ideas, the use of concepts, clarity, and style. We will use
 * undisclosed automatic tests during grading, but not to compute the
 * final grade, but to help us debug your code.
 *
 * We do require that the file compiles.  The directory has a project
 * setup so compilation with scala-cli shall work out-of-the-box.
 * If you cannot make a fragment compile, put your solution in a
 * comment, next to the three question marks. We will grade the
 * solutions in comments as well.
 *
 * Files that do not compile will automatically fail the exam. 
 *
 * The size of the exam has been adjusted from previous years so that
 * you have time to work with the compiler. We do not recommend to run
 * and test the code if you are pressed for time. It is a good idea to
 * run and test, if you have time. 
 *
 * Good luck! */

package adpro

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.TripleEquals.*

import adpro.laziness.LazyList
import adpro.state.*

object Streaming: 

  /* QUESTION 1 ######################################################
   * Study the following recursive function. Then implement the same
   * semantics using a fold.
   */

  def fViaRec (l: LazyList[Int]): Int = 
    def doRec (l: LazyList[Int], z: Int): Int = 
      l match 
        case LazyList.Cons(hd, tl) => 
          if hd() % 2 == 1 
          then doRec(tl(), z+1) 
          else doRec(tl(), z)
        case LazyList.Empty => z

    doRec (l,0)


  def fViaFold (l: LazyList[Int]): Int = 
    l.foldRight(0)((hd, acc) => if hd % 2 == 1 then acc + 1 else acc)

end Streaming



object Parsing:

  import adpro.parsing.*
  import adpro.parsing.Sliceable.*

  /* QUESTION 2 ######################################################
   * The following parser parses CSV (comma separated values) input
   * containing of integer numbers (and  no other types). The type of
   * values produced by a successful parse is List[List[Int]].
   *
   * Example input:
   * 1,2,3 , 5,4
   * 42,42
   *
   * Example output: 
   * List(List(1,2,3,5,4), List(42,42))
   *
   * Use this to write a parser `longestLine` of type Parser[Int] that
   * returns the maximum number of columns in the longest line in the
   * input, measured by the number of integers in the line. The parser
   * should produce Right(5) for the above example.
   */

  val WS: Parser[String] = regex("""[ \t]+""".r) //This parses Whitespaces.
  val NL: Parser[String] = string("\n") //This parses linjeskift/line changes
  val INT: Parser[Int] = 
    regex("""(\+|-)?[0-9]+""".r).map { _.toInt } //Parses an integer I think

  val commaSeparatedInts: Parser[List[Int]] =
    { WS.? |* INT *| WS.? ** (string(",") |* WS.? |* INT *| WS.?).* }
      .map { (h,t) => h::t }
  // WS.? |* INT *| WS.? parses an integer whilst ignoring any whitespaces the |* discards the left side and *| discards the right ergo the whitespaces
  // we then if we have a parsed string, use the combine operator ** to build a tuple which becomes our list and we then see
  // if there is a comma, with more characters afterwards, where we again disregard anything but the int we parse, 
  // and the .* at the end simply is the many operator, so we do this until it fails I assume.

  lazy val parser: Parser[List[List[Int]]] =
    { commaSeparatedInts ** { ( NL |* commaSeparatedInts ) }.* }
      .map { (h,t) => h::t }
  // We take our list of commaseperatedints and combine it with another one, where we first use NL to parse a change of lines
  // disregarding the actual parsing of it and then using commaSperatedInts again to parse the next line, again using .* 
  // to apply this until it fails, aka parsing all lines.

  lazy val longestLine: Parser[Int] = 
    {  parser.map { l => l.map {_.size}.max } }
  // Use map on parser to access the List[List[Int]] which we now have in our l. We then map on l to access a specific List[Int]
  // the _.size call transforms our List[List[Int]] into a List[Int] aka a list of lengths, 
  // and we then call max on this list of lengts to find the biggest int in that list.
  // we return that. The parser definition automatically wraps in a Right on success. 


  /* QUESTION 3 ######################################################
   * Implement a parser of type Parser[Boolean] that parses a CSV
   * input and returns true if all lines have the same number of
   * elements.  It should return Right(False) for the above example.
   *
   * NB. This question does not require that you answered QUESTION 2.
   */

  val allLinesTheSame: Parser[Boolean] = 
    {parser.map { l => l.map {_.size}.toSet.size == 1} }
  // We again convert our List[List[Int]] into a List[Int] with _.size and this time we call .toSet on this to convert it into
  // a set. If all lengts were the same number, the size of the set should be 1 which would return true, otherwise false will
  // be returned by the toSet.size == 1 check. 

end Parsing



object Game:

  import pigaro.*

  /* QUESTION 4 ######################################################
   * Consider the old Chinese game of rock, paper, and scissors.  Two
   * players independently and simultaneously select a move.  The
   * possible moves are Rock, Paper, and Scissors.  The winning is
   * established using the following rules:
   *
   * Rock wins against Scissors
   * Paper wins against Rock
   * Scissors win against Paper
   * Otherwise there is a draw.
   *
   * The function `winner` encodes the rules of the game, i.e. which
   * player wins in each pair of moves. `None` represents a draw.
   * Continue reading below.
   */

  enum Move:
    case Rock, Paper, Scissors

  enum Player: 
    case P1, P2

  import Move.*, Player.*

  type Result = Option[Player]

  def winner(player1: Move, player2: Move): Result = (player1, player2) match
    case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Some(P1)
    case (Scissors, Rock) | (Rock, Paper) | (Paper, Scissors) => Some(P2)
    case _ => None

  /* A strategy is a distribution over the next move, so Dist[Move].
   * Create two strategies, one for Alice and one for Bob:
   * a) Alice picks Rock, Paper, and Scissors with uniform probability.
   * b) Bob Picks Rock and Paper with probability 0.5 each, and never
   * picks Scissors.
   *
   * Link to relevant documentation 
   * https://www.itu.dk/people/wasowski/ula/scala-doc/index.html */

  type Strategy = Dist[Move]

  lazy val Alice: Strategy =
    Pigaro.uniform("Alice")(Rock, Paper, Scissors)
    // Uniform(Rock, Paper, Scissors)

  lazy val Bob: Strategy =
    Pigaro.uniform("Bob")(Rock, Paper)
    // Uniform(Rock, Paper)



  /* QUESTION 5 ######################################################
   * Implement a function that computes a result of a game given two
   * probabilistic strategies `player1` and `player2`. The function
   * shall give a probability of winning for each player, and a
   * probability of a draw as a Dist[Result].   You can use the
   * function 'winner' from above. 
   *
   * Answering QUESTION 4 is not required to answer this one.
   */
  def game (player1: Strategy, player2: Strategy): Dist[Result] =
    player1.map2(player2)(winner) 
    // Takes a move from player 1 and a move from player 2 and applies the winner function to both, 
    // and puts result in new Dist box
    
    (player1 -> player2).map{case (m1, m2) => winner(m1, m2)}
    // Makes a distribution of pairs of the moves player1 and player2 can do, these are then mapped transforming each pair.
    // Case matching is then used to pull out the moves from the pair and give them to the winner function.
    



  /* QUESTION 6 ######################################################
   * Obtain a sample of 10000 outcomes and  estimate the  probability
   * that Alice is winning when playing against  Bob.  You do not have
   * to report the number, just show the code that computes it.
   *
   * The correct answer will compile even if you leave QUESTIONS 4-5 
   * unanswered.
   */
  
  given rng: spire.random.rng.SecureJava 
    = spire.random.rng.SecureJava.apply

  lazy val aliceFraction: Double = 
    game(Alice, Bob).sample(10000).pr(Some(P1))
    // We write Some(P1) instead of just Alice, as that is how it is defined in the winner function.
    // If instead I had written game(Bob, Alice) I would have to have written Some(P2) for it to still calculate the correct thing.

end Game



object RL: 

  /* QUESTION 7 ######################################################
   * The type Q below represents Q-Tables in reinforcement learning,
   * so mapping states to actions to expected rewards. The function
   * update below produces a new q-table which has been modified for
   * the given state and action, using a new value of rewards and
   * estimate.  The details of how this update happens and how it is 
   * used in learning are not relevant for us here though.
   *
   * Our goal is to test the update function, and we will be testing
   * it on the State=Int, Action=Int case. We only test an update for
   * a state and action for which the Q-table q is defined.
   *
   * The type of the specialized update is written below as
   * `IntUpdate` for convenience.  Continue reading below.
   * */

  type Q[State, Action] = Map[State, Map[Action, Double]]

  val α = 0.5
  val γ = 1.0

  def update[State, Action](q: Q[State, Action], state: State, action: Action) 
    (reward: Double, estimate: Double): Q[State, Action] = 
    val qsa   = q(state)(action)
    val value = (1.0 - α) * qsa + α * (reward + γ * estimate) /* MARK */
    val av    = q(state) + (action -> value)
    q + (state -> av)

  type IntUpdate = 
    (Q[Int, Int], Int, Int) => (Double, Double) => Q[Int, Int]

  /* We need test data. We will test on q-tables initialized with zero
   * reward values for all actions and states.  The function qZero
   * below fills a Q-table of size nStates by nActions with zeroes.
   * States are counted from 0 to nStates-1 inclusively, and actions
   * are counted from 0 to nActions-1 inclusively.
   *
   * Continue reading below.
   */

  def qZero(nStates: Int, nActions: Int): Q[Int, Int] = 
    val actionMap: Map[Int, Double] = List.fill(nActions)(0.0)
        .zipWithIndex
        .map(_.swap)
        .toMap
    List.fill(nStates)(actionMap)
      .zipWithIndex
      .map(_.swap)
      .toMap

  /* We will also test on randomly initialized qTables, which are
   * created using the Scalacheck generator below.
   * Continue reading below.
   */

  val genRewards = Gen.choose(-100, 100).map(_.toDouble)
  def qGen(nStates: Int, nActions: Int): Gen[Q[Int, Int]] = for 
    av <- Gen.listOfN[Double](nActions, genRewards)
    av1 = av.zipWithIndex
            .map(_.swap)
            .toMap
    sav = List.fill(nStates)(av1)
              .zipWithIndex
              .map(_.swap)
              .toMap
  yield sav


  abstract class NullUpdatesSpec(update: IntUpdate, name: String) 
    extends org.scalacheck.Properties(name): 

    /* Write a **scenario** test that updates a zero-initialized 2 x 3
     * q-table (States in 0,1; Actions in 0,1,2) on the position
     * (0)(0) with arguments (reward=0.0, estimate=0.0). The test
     * should check that the obtained q-table is still the same as the
     * input table (such operation does nothing interesting).
     */

    property("00 Null update on null table 2x3") = 
      update(qZero(2,3), 0, 0)(0.0, 0.0) == qZero(2,3)
    /* 
    * Just do exactly as the task is written. First update, so we call the update function. 
    * Then it should be a 2 x 3 zero-initialized q table which the qZero function makes, so we call that with (2,3)
    * to get the size that is asked for.
    * Then it should be called with position 0, 0 so we pass these along.
    * It should be with reward 0.0 and estimate 0.0 so we pass that in a new () as it is done in the update function.
    * Finally we should check that the table is equal to the input table ergo qZero(2,3).
    */ 



    /* QUESTION 8 ####################################################
     * Write a **property** test that runs on randomly initialized
     * Q-Tables of size 2 x 3. The update should be performed on any
     * position (for any state, and any action within the range
     * defined in the Q-table), not only at (0)(0) as above. 
     *
     * The `reward` argument should get the value stored in the input
     * Q-table on the updated position. The estimate argument should
     * still be zero.
     *
     * Check that the resulting table under these conditions is the
     * same as the input table.
     */

    property("01 Null update on null table 2x3") = 
      forAll(qGen(2,3)) { q => 
        forAll(Gen.choose(0, 1), Gen.choose(0, 2)) { (state, action) => 
          val qUpdate = update(q, state, action)(q(state)(action), 0.0)
          qUpdate ?= q //using ?= gives more information on why it fails as it attaches labels to each side of the comparison
          // much better than simply using == which just gives a true or false value.
        }
      }

  end NullUpdatesSpec

  import monocle.*
  import monocle.syntax.all.*
  import monocle.function.all.*
  import monocle.function.Index
  import monocle.PLens.lensChoice

  /* QUESTION 9 ######################################################
   * We want to rewrite the update function using lenses.  The lens
   * `lens` below, implemented using the monocle library, projects a
   * q-table at its value at position (s,a) (i.e, state and action). 
   *
   * Reimplement the function 'update' to use this lens. Note that the
   * lens is used to obtain and replace the reward value. The actual
   * update calculation (marked MARK in the update function above)
   * will not change in this implementation, only the code to access
   * and replace the value.
   *
   * Assume that we only perform an update for a state and action for
   * which the Q table is defined.
   *
   * This question can be answered independently of the previous one.
   */

  def lens[S, A](s: S, a: A): Optional[Q[S,A], Double] = 
    val l1 = summon[Index[Q[S, A], S, Map[A, Double]]].index(s)
    val l2 = summon[Index[Map[A, Double], A, Double]].index(a)
    l1.andThen(l2)
  
  def updateWithLens[State, Action] (q: Q[State, Action], s: State, a: Action)
    (reward: Double, estimate: Double): Q[State, Action] =
    val qsa = lens(s, a).getOption(q).get
    val value = (1.0 - α) * qsa + α * (reward + γ * estimate)
    lens(s, a).replace(value)(q)

end RL
