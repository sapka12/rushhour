package hu.arnoldfarkas.rushhour.game

trait GameSolver[S, M] {

  val actions: Set[M]

  def step(gameState: S, move: M): Option[S]

  def isFinal(gameState: S): Boolean

  private type Path = List[M]
  private type History = (S, Path)
  private type HistoryLayer = Set[History]

  private def nextLayer(historyLayer: HistoryLayer, visited: Set[S]): (HistoryLayer, Set[S]) = {

    val numOfStatesInLayer = historyLayer.size
    println(s"numOfStatesInLayer: $numOfStatesInLayer")
    println(s"visited: ${visited.size}")
    println()

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

    val nextLayer: HistoryLayer = layer.seq.groupBy{
      case (s, _) => s
    }.mapValues(_.map(_._2).reduce[Path]{
      case (ms, ms2) => if (ms.size < ms2.size) ms else ms2
    }).toSet.seq

    (nextLayer, allVisited)
  }

  private def tree[A](historyLayer: HistoryLayer, visited: Set[S], count: Int = 0): Stream[HistoryLayer] =
    if (historyLayer isEmpty) Stream.empty[HistoryLayer]
    else {
      println(s"layer: $count")
      val (nextHistoryLayer, nextStates) = nextLayer(historyLayer, visited)
      historyLayer #:: tree(nextHistoryLayer, visited ++ nextStates, count + 1)
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
    solution(build(startState)).headOption.map(_.reverse)
}
