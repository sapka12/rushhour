package hu.arnoldfarkas.rushhour.game

object GameTree {

  def nextHistory(histories: Stream[History]): Option[History] = {

    val inPrevious: History => Boolean = h => histories.map(_.state).contains(h.state)

    def optionalHistory(histories: Stream[History]): Option[History] = histories match {
      case Stream() => None
      case history #:: otherHistories => {

        val nextHistories = history.next()
          .filterNot(inPrevious)

        if (nextHistories.isEmpty) {
          optionalHistory(otherHistories)
        } else {
          Some(nextHistories.head)
        }
      }
    }

    optionalHistory(histories.reverse)
  }

  def tree[A](visited: Stream[A], next: Stream[A] => Option[A]): Stream[A] = {
    next(visited) match {
      case None => visited
      case Some(nextElement) => tree(nextElement #:: visited, next)
    }
  }

  def build(startState: State): GameTree = {
    val initialHistory = History(startState, Path.empty)
    val histories = tree(Stream(initialHistory), nextHistory)
    GameTree(histories)
  }
}

case class GameTree(histories: Stream[History]) {
  def solution(finalCarPos : Car): Option[Path] =
    histories
      .filter(h => State.isFinal(h.state, finalCarPos))
      .sortWith((lt, gt) => lt.path.moves.size < gt.path.moves.size)
      .headOption.map(_.path)
}
