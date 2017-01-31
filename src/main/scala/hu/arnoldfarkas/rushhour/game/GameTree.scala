package hu.arnoldfarkas.rushhour.game

object GameTree {

  def nextHistory(histories: List[History]): Option[History] = {

    val visited = histories.map(_.state)

    def optionalHistory(histories: List[History]): Option[History]  = histories
      .sortWith((lt, gt) => lt.path.moves.size < gt.path.moves.size)
    match {
      case List() => None
      case history :: otherHistories => {

        val nextHistories = history
          .next()
          .filterNot(h => visited.contains(h.state))

        if (nextHistories.isEmpty) {
          optionalHistory(otherHistories)
        } else {
          Some(nextHistories.head)
        }
      }
    }

    optionalHistory(histories)
  }

  def tree[A](visited: List[A], next: List[A] => Option[A]): List[A] = {
    next(visited) match {
      case None => visited
      case Some(elem) => tree(elem :: visited, next)
    }
  }

  def build(startState: State): GameTree = {
    val initialHistory = History(startState, Path.empty)
    val histories = tree(List(initialHistory), nextHistory)
    GameTree(histories)
  }
}

case class GameTree(histories: List[History]) {
  def solution(finalCarPos : Car): Option[Path] =
    histories
      .filter(h => State.isFinal(h.state, finalCarPos))
      .sortWith((lt, gt) => lt.path.moves.size < gt.path.moves.size)
      .headOption.map(_.path)
}
