package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.Game._
import scala.io.Source

object RushHourApp {

  def main(args: Array[String]): Unit = {
    val inputFilename = args(0)
    val finalCarPosition: Car = parseCar(args(1)) // a,0,2,1,2

    val finalState: FinalState = GameFactory.finalState(finalCarPosition)
    val file = Source.fromFile(inputFilename)
    val startState: State = GameFactory.state(file.mkString, "abc".toSet, 'x')

    def buildGameTree(initState: State): List[(State, Path)] = {

      def validMoves(state: State): Set[(Move, State)] = ???


      def gameTree(visitedStates: List[(State, Path)]): List[(State, Path)] = {

        val state = visitedStates.head._1
        val path = visitedStates.head._2

        def existIn(moveAndState: (Move, State), visitedStates: List[(State, Path)]) =
          visitedStates.map(_._1).contains(moveAndState._2)

        val nextMoves: Set[(Move, State)] =
          validMoves(visitedStates.head._1).filterNot(existIn(_, visitedStates))

        if (nextMoves isEmpty) {
          visitedStates
        } else {
          for {
            (newMove, newState) <- nextMoves
            gameTree((newState, ) :: visitedStates)
          }
        }
      }

      gameTree(initState, List())
    }

//    val gameTree: List[(State, History)] = List((startState, List.empty[Move]))

    val solution: Path = ???

    println(startState)
  }

  def parseCar(inputCar: String): Car = {
    val split = inputCar.split(",")
    val id = split.head(0)
    val groupedPos: Set[Array[String]] = split.tail.grouped(2).toSet
    val positions = groupedPos.map(p => Pos(p(0).toInt, p(1).toInt))
    (positions, id)
  }
}