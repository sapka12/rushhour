package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game.GameTree._

object GameTree {

  type HistoryLayer = Set[History]

  def tree[A](historyLayer: HistoryLayer, visited: Set[State]): Stream[HistoryLayer] = {

    if (historyLayer isEmpty) Stream.empty[HistoryLayer]
    else {
      val nextLayer = (
        for {
          history <- historyLayer
          nextHistory <- history.next()
          if (!visited.contains(nextHistory.state))
        } yield nextHistory
        )
        .groupBy(_.state)

      val historiesInNextLayer = nextLayer.values.map(_.head).toSet

      historyLayer #:: tree(historiesInNextLayer, visited ++ nextLayer.keySet)
    }
  }

  def build(startState: State): GameTree = {
    val initialHistory = History(startState, Path.empty)
    val histories = tree(Set(initialHistory), Set(startState))
    GameTree(histories)
  }
}

case class GameTree(histories: Stream[HistoryLayer]) {
  def solution(finalCarPos: Car): Stream[Path] = for {
    historyLevel <- histories
    history <- historyLevel
    if (State.isFinal(history.state, finalCarPos))
  } yield history.path
}
