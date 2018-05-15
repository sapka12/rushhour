package hu.arnoldfarkas.rushhour.bridgeriddle

import hu.arnoldfarkas.rushhour.bridgeriddle.BridgeSolver.BridgeStep
import hu.arnoldfarkas.rushhour.game.GameSolver

case class BridgeGameState(usersAtStart: Set[Int], usersAtFinish: Set[Int], timeSpent: Int, lampOnStart: Boolean)

object BridgeSolver {
  type BridgeStep = List[Int]
}

class BridgeSolver(val users: List[Int], maxTime: Int) extends GameSolver[BridgeGameState, BridgeStep]{

  override val actions: Set[BridgeStep] = (
    for {
      user1 <- users
      user2 <- None :: users.map(Some(_))
      if Some(user1) != user2
    } yield List(Some(user1), user2).flatMap(_.toList)
    ).toSet

  override def step(gameState: BridgeGameState, move: BridgeStep): Option[BridgeGameState] = gameState match {
    case BridgeGameState(usersAtStart, usersAtFinish, timeSpent, lampOnStart)
      if lampOnStart && move.max + timeSpent > maxTime =>None
    case BridgeGameState(usersAtStart, usersAtFinish, timeSpent, lampOnStart)
      if lampOnStart && move.forall(usersAtStart.contains(_)) =>
      Some(BridgeGameState(usersAtStart -- move, usersAtFinish ++ move, timeSpent + move.max, false))
    case BridgeGameState(usersAtStart, usersAtFinish, timeSpent, lampOnStart)
      if !lampOnStart && move.forall(usersAtFinish.contains(_)) =>
      Some(BridgeGameState(usersAtStart ++ move, usersAtFinish -- move, timeSpent + move.max, true))
    case _ => None
  }

  override def isFinal(gameState: BridgeGameState): Boolean = gameState.usersAtStart.isEmpty
}

object RunBridgeRiddle extends App {
  val solver = new BridgeSolver(List(1, 2, 5, 10), 17)
  val solution = solver.solve(BridgeGameState(solver.users.toSet, Set(), 0, true))
  println(solution)
}
