package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.rushhour._
import org.scalatest.{FlatSpec, Matchers}

class RushHourGameSolverSpec extends FlatSpec with Matchers {
  behavior of "step"

  it should "have proper set eq" in {
    assert(Set(1, 2, 3) == Set(3, 2, 1))
    assert(Set(1, 2, 3, 4, 5) == Set(5, 4, 3, 2, 1))
  }

  behavior of "solver"

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

    val solver = new RushHourGameSolver(startState.cars, finalCarPosition)

    val solution = solver.solve(startState)
    val moves: List[RushHourMove] = solution.get

    moves.size should be <= maxNumOfStepsInSolution
  }

  it should "solve 2 step" in {

    val maxNumOfStepsInSolution = 2

    val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'X')
    val startState: RushHourState = GameFactory.state(
      """xxxxxx
        |xxxxxa
        |xxxXXa
        |xxxxxx
        |xxxxxx
        |xxxxxx
      """.stripMargin, 'x')

    val solver = new RushHourGameSolver(startState.cars, finalCarPosition)

    val solution = solver.solve(startState)
    val moves: List[RushHourMove] = solution.get

    println(solution)

    moves.size should be <= maxNumOfStepsInSolution
  }

  it should "solve 4 step" in {

    val maxNumOfStepsInSolution = 4

    val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'X')
    val startState: RushHourState = GameFactory.state(
      """xxeddd
        |xxexxa
        |xxxXXa
        |xxcbbb
        |xxcxxx
        |xxxxxx
      """.stripMargin.filterNot(_ == '\r'), 'x')

    val solver = new RushHourGameSolver(startState.cars, finalCarPosition)

    val solution = solver.solve(startState)
    val moves: List[RushHourMove] = solution.get

    println(solution)

    moves.size should be <= maxNumOfStepsInSolution
  }

  it should "solve level6 from TrafficHour" in {

    val maxNumOfStepsInSolution = 14

    val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'X')
    val startState: RushHourState = GameFactory.state(
      """xxxxfg
        |aaxxfg
        |XXxxfg
        |bbxxxx
        |cxxxxx
        |cddeex
      """.stripMargin, 'x')

    val solver = new RushHourGameSolver(startState.cars, finalCarPosition)

    val solution = solver.solve(startState)
    val moves: List[RushHourMove] = solution.get

    println(solution)

    moves.size should be <= maxNumOfStepsInSolution
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

    val solver = new RushHourGameSolver(startState.cars, finalCarPosition)

    val solution = solver.solve(startState)
    val moves: List[RushHourMove] = solution.get

    moves.size should be <= maxNumOfStepsInSolution
  }

  it should "not give a solution when there is no" in {

    val impossibleCarPosition: Car = Car(Set(Pos(1, 1), Pos(1, 2)), 'a')
    val startState: RushHourState = GameFactory.state(
      """aax
        |xxx
      """.stripMargin, 'x')

    val solver = new RushHourGameSolver(startState.cars, impossibleCarPosition)

    val solution = solver.solve(startState)

    solution shouldBe None
  }

}
