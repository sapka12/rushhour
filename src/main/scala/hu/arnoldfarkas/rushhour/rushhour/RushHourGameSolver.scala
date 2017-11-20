package hu.arnoldfarkas.rushhour.rushhour

import hu.arnoldfarkas.rushhour.game._
import hu.arnoldfarkas.rushhour.common.GameSolver
import hu.arnoldfarkas.rushhour.game.{Car, RushHourMove, RushHourState}

class RushHourGameSolver(cars: Set[Car], finalCarPos: Car) extends GameSolver[RushHourState, RushHourMove] {

  override val moves: Set[RushHourMove] = for {
    car <- cars
    step <- Set(Up, Down, Left, Right)
  } yield RushHourMove(car, step)

  override def step(gameState: RushHourState, move: RushHourMove): Option[RushHourState] =
    {
      val valid = gameState
        .validMoves

      println()
      println()
      //TODO fix Set[Pos]
      println("VALID:")
      println(s"$move")
      println(s"$valid")
      println()
      println()
      println()
      println()

      valid.filter{
          case (car, rhMove, state) =>
            val eqPos = move.car.positions.sameElements(rhMove.car.positions)
            val eqSign = move.car.sign == rhMove.car.sign
            val eqStep = move.step == rhMove.step
            val eq = eqPos && eqSign && eqStep
            println(s"$move == $rhMove  ===>  $eq  [$eqPos, $eqSign, $eqStep]")
            eq
        }
        .map{
          case (car, rhMove, state) => state
        }
        .toList.headOption
    }

  override def isFinal(gameState: RushHourState) = RushHourState.isFinal(gameState, finalCarPos)
}
