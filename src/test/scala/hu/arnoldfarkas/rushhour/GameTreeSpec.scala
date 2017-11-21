package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._
import org.scalatest.{FlatSpec, Matchers}

class GameTreeSpec extends FlatSpec with Matchers {


  it should "solve 1 step" in {

    val maxNumOfStepsInSolution = 1

    val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'X')
    val startState: RushHourState = GameFactory.state(
      """xxxxxx
        |xxxxxx
        |xxxXXx
        |xxxxxx
        |xxxxxx
        |xxxxxx
      """.stripMargin, 'x')

    val solution = GameTree.solve(startState, finalCarPosition)

    solution.get.moves.size should be <= maxNumOfStepsInSolution
  }

  it should "solve the 40th card from RushHourJunior" in {

    val maxNumOfStepsInSolution = 38

    val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'X')
    val startState: RushHourState = GameFactory.state(
      """aaxxbo
        |cdxxbo
        |cdxXXo
        |pxxexx
        |pffegg
        |phhqqq
      """.stripMargin, 'x')

    val solution = GameTree.solve(startState, finalCarPosition)

    solution.get.moves.size should be <= maxNumOfStepsInSolution
  }

  it should "not give a solution when there is no" in {

    val impossibleCarPosition: Car = Car(Set(Pos(1, 1), Pos(1, 2)), 'a')
    val startState: RushHourState = GameFactory.state(
      """aax
        |xxx
      """.stripMargin, 'x')

    GameTree.solve(startState, impossibleCarPosition) shouldBe None
  }
}
