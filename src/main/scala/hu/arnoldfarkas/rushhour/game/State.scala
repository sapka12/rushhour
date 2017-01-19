package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game.Path._

object State {
  def isFinal(state: State, car: Car): Boolean =
    state.cars.contains(car)
}

case class State(val field: Field, val cars: Set[Car]) {

  override def toString: String = "State: "+ cars.toString()

  override def equals(obj: scala.Any): Boolean = obj match {
    case s: State => /*s.field == field &&*/ s.cars == cars
    case _ => false
  }

  def withMove(c: Car, move: Move): State =
    copy(cars = cars - c + move.car)

  def isValid: Boolean = {
    val allPos = cars.toList.flatMap(_.positions)
    allPos.toSet.size == allPos.size
  }

  def validMoves: Set[(Car, Move, State)] =
    for {
      car <- cars
      move <- car.validMoves
      stateByMove = withMove(car, move)
      if (stateByMove.isValid)
      if (stateByMove.cars.flatMap(_.positions).forall(field))
    } yield (car, move, stateByMove)

}
