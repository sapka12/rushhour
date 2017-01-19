package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game.Path.{History, Move}

object GameTree {

  def nextHistory(histories: List[History]): Option[History] = histories match {
    case List() => None
    case rootHistory :: otherHistories => {
      def visited(m: (Car, Move, State)): Boolean = {
          histories.map(_._1).contains(m._3)
      }

      val (rootState, rootPath) = rootHistory
      val possibleSteps = rootState.validMoves.filterNot(visited)

      if (possibleSteps.isEmpty) {

        nextHistory(otherHistories)
      } else {
        val (_, nextMove, nextState) = possibleSteps.head
        Some((nextState, nextMove :: rootPath))
      }
    }
  }

  def build(startState: State): List[History] = {

    def gameTree(visitedStates: List[History]): List[History] = {
      nextHistory(visitedStates) match {
        case None => visitedStates
        case Some(nextState) => {
          gameTree(nextState :: visitedStates)
        }
      }
    }

    gameTree(List((startState, List())))
  }
}
