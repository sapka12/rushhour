package hu.arnoldfarkas.rushhour.rushhour

import hu.arnoldfarkas.rushhour.game._
import hu.arnoldfarkas.rushhour.common.GameSolver
import hu.arnoldfarkas.rushhour.game.{Car, RushHourMove, RushHourState}

class RushHourGameSolver(cars: Set[Car], finalCarPos: Car) extends GameSolver[RushHourState, RushHourMove] {

  override val actions: Set[RushHourMove] = for {
    car <- cars
    step <- Set(Up, Down, Left, Right)
    if car.validStep(step)
  } yield RushHourMove(car.sign, step)

  override def step(gameState: RushHourState, move: RushHourMove): Option[RushHourState] = {

    implicit val field = gameState.field

    val movingCar = gameState.cars.filter(_.sign == move.carSign).toList.head
    val movedCar = move.step match {
      case Up => movingCar.copy(positions = movingCar.positions.map(p => Pos(p.x, p.y - 1)))
      case Down => movingCar.copy(positions = movingCar.positions.map(p => Pos(p.x, p.y + 1)))
      case Left => movingCar.copy(positions = movingCar.positions.map(p => Pos(p.x - 1, p.y)))
      case Right => movingCar.copy(positions = movingCar.positions.map(p => Pos(p.x + 1, p.y)))
    }

    val otherCars = gameState.cars.filter(_.sign != movingCar.sign)

    if (movedCar.inField(gameState.field) && otherCars.forall(!movedCar.hasCommonPos(_)))
      Some(gameState.copy(cars = otherCars + movedCar))
    else
      None
  }

  override def isFinal(gameState: RushHourState) = RushHourState.isFinal(gameState, finalCarPos)
}
