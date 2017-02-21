package hu.arnoldfarkas.rushhour.game

object GameTree {

  def tree[A](historiesInLayer: Set[History], visited: Set[State], layer: Int): Stream[Set[History]] = {

    if (historiesInLayer isEmpty) Stream.empty[Set[History]]
    else {
      val historiesInNextLayer = (for {
        history <- historiesInLayer
        nextHistory <- history.next()
        if (!visited.contains(nextHistory.state))
      } yield nextHistory)
        //TODO
        .groupBy(_.state).values.map(_.head).toSet

      val allVisited = historiesInNextLayer.map(_.state) ++ visited

      historiesInLayer #:: tree(historiesInNextLayer, allVisited, layer + 1)
    }
  }

  def build(startState: State): GameTree = {
    val initialHistory = History(startState, Path.empty)
    val histories = tree(Set(initialHistory), Set(startState), 0)
    GameTree(histories)
  }
}

case class GameTree(histories: Stream[Set[History]]) {
  def solution(finalCarPos: Car): Stream[Path] = for {
    historyLevel <- histories
    history <- historyLevel
    if (State.isFinal(history.state, finalCarPos))
  } yield history.path
}
