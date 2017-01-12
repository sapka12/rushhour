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

      def validSteps(c: Car): Set[Move] = c._1 match {
        case positions if positions.map(_.x).forall(_ == positions.head.x) => {
          Set(
            ((c._1.map(pos => Pos(pos.x, pos.y - 1)), c._2), Up),
            ((c._1.map(pos => Pos(pos.x, pos.y + 1)), c._2), Down)
          )
        }
        case positions if positions.map(_.y).forall(_ == positions.head.y) => {
            Set(
              ((c._1.map(pos => Pos(pos.x-1, pos.y)), c._2), Left),
              ((c._1.map(pos => Pos(pos.x+1, pos.y)), c._2), Right)
            )
        }
        case _ => throw IllegalArgumentException
      }

      def validMoves(state: State): Set[(Move, State)] = {
        val actualCars: Set[Car] = state._2

      }

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
            (newMove, newState) <- nextMoves.toList
            tree <- gameTree((newState, newMove :: path) :: visitedStates)
          } yield tree
        }
      }

      gameTree(List((initState, List())))
    }

    val gameTree = buildGameTree(startState)

    val solution: Path = gameTree.filter(a => finalState())

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
