package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._
import org.scalatest.{FlatSpec, Matchers}

class GameTreeSpec extends FlatSpec with Matchers {

  behavior of "solve"

  it should "solve the 40th card from RushHourJunior" in {

    val maxNumOfStepsInSolution = 38

    val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'X')
    val startState: State = GameFactory.state(
      """aaxxbo
        |cdxxbo
        |cdxXXo
        |pxxexx
        |pffegg
        |phhqqq
      """.stripMargin
      , "abcdoXpfhegq".toSet, 'x')

    val solution = GameTree.solve(startState, finalCarPosition)

    solution.get.moves.size should be <= maxNumOfStepsInSolution
  }

  it should "not give a solution when there is no" in {

    val impossibleCarPosition: Car = Car(Set(Pos(1, 1), Pos(1, 2)), 'a')
    val startState: State = GameFactory.state(
      """aax
        |xxx
      """.stripMargin
      , "a".toSet, 'x')

    GameTree.solve(startState, impossibleCarPosition) shouldBe None
  }
}
