package hu.arnoldfarkas.rushhour.game

object GameTree {

  def nextHistory(histories: List[History]): Option[History] = histories match {
    case List() => None
    case history :: otherHistories => {
      def visited(m: (Car, Move, State)): Boolean = {
          histories.map(_.state).contains(m._3)
      }

      val possibleSteps = history.state.validMoves.filterNot(visited)

      if (possibleSteps.isEmpty) {

        nextHistory(otherHistories)
      } else {
        val (_, nextMove, nextState) = possibleSteps.head
        Some(History(nextState, Path(nextMove :: history.path.moves)))
      }
    }
  }

  def build(startState: State): GameTree = {

    def gameTree(histories: List[History]): List[History] = {
      nextHistory(histories) match {
        case None => histories
        case Some(nextState) => {
          gameTree(nextState :: histories)
        }
      }
    }

    val initialHistory = History(startState, Path.empty)

    GameTree(gameTree(List(initialHistory)))
  }
}

case class GameTree(histories: List[History])
