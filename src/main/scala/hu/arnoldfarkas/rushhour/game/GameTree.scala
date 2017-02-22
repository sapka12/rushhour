package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game.GameTree._

object GameTree {

  type HistoryLayer = Set[History]

  private def tree[A](historyLayer: HistoryLayer, visited: Set[State]): Stream[HistoryLayer] =
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

  def build(startState: State): Stream[History] =
    tree(
      Set(History(startState, Path.empty)),
      Set(startState)
    ).flatten

  def solution(histories: Stream[History], finalCarPos: Car): Stream[Path] =
    histories
      .filter(history => State.isFinal(history.state, finalCarPos))
      .map(_.path)

  def solve(startState: State, finalCarPos: Car): Option[Path] =
    solution(build(startState), finalCarPos).headOption
}
