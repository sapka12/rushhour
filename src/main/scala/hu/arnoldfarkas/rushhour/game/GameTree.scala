package hu.arnoldfarkas.rushhour.game

object GameTree {

  type HistoryLayer = Set[History]

  private def nextLayer(historyLayer: HistoryLayer, visited: Set[State]): (HistoryLayer, Set[State]) = {

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

  private def tree[A](historyLayer: HistoryLayer, visited: Set[State]): Stream[HistoryLayer] =
    if (historyLayer isEmpty) Stream.empty[HistoryLayer]
    else {
      val (nextHistoryLayer, nextStates) = nextLayer(historyLayer, visited)
      historyLayer #:: tree(nextHistoryLayer, visited ++ nextStates)
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
