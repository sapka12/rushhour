package hu.arnoldfarkas.rushhour

import Game._

object Solver {

  def findNextPath(paths: List[Path]) = {

  }

  def solve(start: State, carEndPos: Car): Option[Path] = {

    val isFinal = GameFactory.finalState(carEndPos)

    def findAllPaths(paths: List[Path]): List[Path] = {
      val next: Option[Path] = findNextPath(paths)
      if (next isEmpty) paths
      else findAllPaths(next.get :: paths)
    }

    val allPaths = findAllPaths(List(List(start)))

    allPaths.find(p => isFinal(p(0)))
  }
}
