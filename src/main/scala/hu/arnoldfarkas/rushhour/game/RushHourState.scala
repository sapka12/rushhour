package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game._

object RushHourState {
  def isFinal(state: RushHourState, car: Car): Boolean =
    state.cars.contains(car)
}

case class RushHourState(val cars: Set[Car])(implicit field: Field) {

  override def toString: String = "State: "+ cars.toString()

  override def equals(obj: scala.Any): Boolean = obj match {
    case s: RushHourState => /*s.field == field &&*/ s.cars == cars
    case _ => false
  }

  private def withMove(c: Car, move: RushHourMove): RushHourState =
    copy(cars = cars - c + move.car)

  private def isValid: Boolean = {
    val allPos = cars.toList.flatMap(_.positions)
    allPos.toSet.size == allPos.size
  }

  def validMoves: Set[(Car, RushHourMove, RushHourState)] =
    for {
      car <- cars
      move <- car.validMoves
      stateByMove = withMove(car, move)
      if (stateByMove.isValid)
      if (stateByMove.cars.flatMap(_.positions).forall(field.f))
    } yield (car, move, stateByMove)

}
