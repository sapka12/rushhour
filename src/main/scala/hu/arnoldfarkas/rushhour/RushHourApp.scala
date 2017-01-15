package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game.Path._
import hu.arnoldfarkas.rushhour.game._

import scala.io.Source

object RushHourApp {

  def main(args: Array[String]): Unit = {
    val inputFilename = args(0)
    val finalCarPosition: Car = parseCar(args(1)) // a,0,2,1,2

    val file = Source.fromFile(inputFilename)
    val startState: State = GameFactory.state(file.mkString, "abc".toSet, 'x')

    def buildGameTree(initState: State): List[(State, Path)] = {

      def gameTree(visitedStates: List[(State, Path)]): List[(State, Path)] = {

        val previousState = visitedStates.head._1
        val previousPath = visitedStates.head._2

        val moves = previousState.validMoves

        if (moves isEmpty) visitedStates
        else {

          val nextStuff: List[(State, Path)] = for {
            (move, state) <- moves
            nextState: State <- state.withMove(move)
            list <- gameTree((nextState, move :: previousPath) :: visitedStates)
          } yield list

          nextStuff.filterNot(visitedStates.contains(_)) ::: visitedStates
        }
      }

      gameTree(List((initState, List())))
    }

    val gameTree = buildGameTree(startState)

    val solution: Path =
      gameTree.filter(p => State.isFinal(p._1, finalCarPosition)).head._2

    println(solution)
  }

  def parseCar(inputCar: String): Car = {
    val split = inputCar.split(",")
    val id = split.head(0)
    val groupedPos: Set[Array[String]] = split.tail.grouped(2).toSet
    val positions = groupedPos.map(p => Pos(p(0).toInt, p(1).toInt))
    Car(positions, id)
  }
}
