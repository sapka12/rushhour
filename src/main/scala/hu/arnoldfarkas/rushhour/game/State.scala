package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game._

object State {
  def isFinal(state: State, car: Car): Boolean =
    state.cars.contains(car)
}

case class State(val cars: Set[Car])(implicit field: Field) {

  override def toString: String = "State: "+ cars.toString()

  override def equals(obj: scala.Any): Boolean = obj match {
    case s: State => /*s.field == field &&*/ s.cars == cars
    case _ => false
  }

  private def withMove(c: Car, move: Move): State =
    copy(cars = cars - c + move.car)

  private def isValid: Boolean = {
    val allPos = cars.toList.flatMap(_.positions)
    allPos.toSet.size == allPos.size
  }

  def validMoves: Set[(Car, Move, State)] =
    for {
      car <- cars
      move <- car.validMoves
      stateByMove = withMove(car, move)
      if (stateByMove.isValid)
      if (stateByMove.cars.flatMap(_.positions).forall(field.f))
    } yield (car, move, stateByMove)

}
