package hu.arnoldfarkas.rushhour.rushhour

import hu.arnoldfarkas.rushhour.game._
import hu.arnoldfarkas.rushhour.common.GameSolver
import hu.arnoldfarkas.rushhour.game.{Car, RushHourMove, RushHourState}

class RushHourGameSolver(carSigns: Set[Char], finalCarPos: Car) extends GameSolver[RushHourState, RushHourMove] {

  override val actions: Set[RushHourMove] = for {
    car <- carSigns
    step <- Set(Up, Down, Left, Right)
  } yield RushHourMove(car, step)

  override def step(gameState: RushHourState, move: RushHourMove): Option[RushHourState] = {

    val car = gameState.cars.filter(_.sign == move.carSign).head

    if (
      (car.isHorizontal && (move.step == Up || move.step == Down)) ||
        (!car.isHorizontal && (move.step == Left || move.step == Right))
    ) None
    else {

      val newStateCars = gameState.cars.map(c =>
        if (c.sign == move.carSign) move.step match {
          case Up => c.copy(positions = c.positions.map(p => Pos(p.x, p.y - 1)))
          case Down => c.copy(positions = c.positions.map(p => Pos(p.x, p.y + 1)))
          case Left => c.copy(positions = c.positions.map(p => Pos(p.x - 1, p.y)))
          case Right => c.copy(positions = c.positions.map(p => Pos(p.x + 1, p.y)))
        }
        else c
      )

      def isValid(cars: Set[Car]): Boolean = {
        val inField = (for {
          car <- cars
          pos <- car.positions
        } yield pos).forall(gameState.field.f(_))


        val noOverlap = (for {
          car <- cars
          car2 <- cars
          if (car != car2)
          if (car hasCommonPos car2)
        } yield car).isEmpty

        inField && noOverlap
      }

      if (isValid(newStateCars)) Some(gameState.copy(cars = newStateCars)(field = gameState.field))
      else None
    }
  }

  override def isFinal(gameState: RushHourState) = RushHourState.isFinal(gameState, finalCarPos)
}
