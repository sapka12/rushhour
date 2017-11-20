package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.rushhour.RushHourGameSolver

object GameTree {

  type HistoryLayer = Set[History]

  private def nextLayer(historyLayer: HistoryLayer, visited: Set[RushHourState]): (HistoryLayer, Set[RushHourState]) = {

    val layer = (
      for {
        history <- historyLayer.par
        nextHistory <- history.next()
        if (!visited.contains(nextHistory.state))
      } yield nextHistory
      )
      .groupBy(_.state).seq

    (layer.values.map(_.head).toSet, layer.keySet)
  }

  private def tree[A](historyLayer: HistoryLayer, visited: Set[RushHourState]): Stream[HistoryLayer] =
    if (historyLayer isEmpty) Stream.empty[HistoryLayer]
    else {
      val (nextHistoryLayer, nextStates) = nextLayer(historyLayer, visited)
      historyLayer #:: tree(nextHistoryLayer, visited ++ nextStates)
    }

  def build(startState: RushHourState): Stream[History] =
    tree(
      Set(History(startState, Path.empty)),
      Set(startState)
    ).flatten

  def solution(histories: Stream[History], finalCarPos: Car): Stream[Path] =
    histories
      .filter(history => RushHourState.isFinal(history.state, finalCarPos))
      .map(_.path)

  def solve(startState: RushHourState, finalCarPos: Car): Option[Path] =
//    solution(build(startState), finalCarPos).headOption
      new RushHourGameSolver(startState.cars, finalCarPos).solve(startState).map(Path(_))
}
