package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.rushhour.{Car, RushHourGameSolver}

import scala.io.Source

object RushHour {

  def main(args: Array[String]): Unit = {
    val startStateFile = args(0)
    val endStateFile = args(1)

    val state = GameFactory.state(
      Source.fromFile(startStateFile).mkString,
      'x'
    )

    val finish: Car = GameFactory.state(
      Source.fromFile(endStateFile).mkString,
      'x'
    ).cars.head

    val gameSolver = new RushHourGameSolver(state.cars, finish)
    val solution = gameSolver.solve(state)

    println(solution)
  }
}
