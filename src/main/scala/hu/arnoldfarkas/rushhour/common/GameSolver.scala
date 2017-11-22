package hu.arnoldfarkas.rushhour.common

trait GameSolver[S, M] {

  val actions: Set[M]

  def step(gameState: S, move: M): Option[S]

  def isFinal(gameState: S): Boolean

  private type Path = List[M]
  private type History = (S, Path)
  private type HistoryLayer = Set[History]

  private def nextLayer(historyLayer: HistoryLayer, visited: Set[S]): (HistoryLayer, Set[S]) = {

    val layer =
      for {
        (state, path) <- historyLayer.par
        (nextState, move) <- actions.map(m => (step(state, m), m))
        if nextState.isDefined
        if !visited.contains(nextState.get)
      } yield (nextState.get, move :: path)

    val allVisited = visited ++ layer.map{
      case (nextState, _) => nextState
    }

    (layer.seq, allVisited)
  }

  private def tree[A](historyLayer: HistoryLayer, visited: Set[S]): Stream[HistoryLayer] =
    if (historyLayer isEmpty) Stream.empty[HistoryLayer]
    else {
      val (nextHistoryLayer, nextStates) = nextLayer(historyLayer, visited)
      historyLayer #:: tree(nextHistoryLayer, visited ++ nextStates)
    }

  private def build(startState: S): Stream[History] =
    tree(
      Set((startState, List.empty[M])),
      Set(startState)
    ).flatten

  private def solution(histories: Stream[History]): Stream[Path] =
    for {
      (state, path) <- histories
      if isFinal(state)
    } yield path

  def solve(startState: S): Option[Path] =
    solution(build(startState)).headOption
}
