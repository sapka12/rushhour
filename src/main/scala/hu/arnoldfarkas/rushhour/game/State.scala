package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game.Path._

object State {
  def isFinal(state: State, car: Car): Boolean =
    state.cars.contains(car)
}

case class State(val field: Field, val cars: Set[Car]) {

  override def toString: String = "State: "+ cars.toString()

  def withMove(move: Move): State =
    copy(cars = cars.filterNot(_.sign == move.car.sign) + move.car)

  def isValid: Boolean = {
    val allPos = cars.toList.flatMap(_.positions)
    allPos.toSet.size == allPos.size
  }

  def validMoves: Set[(Move, State)] =
    for {
      car <- cars
      move <- car.validMoves
      stateByMove = withMove(move)
      if (stateByMove.isValid)
      if (stateByMove.cars.flatMap(_.positions).forall(field))
    } yield (move, stateByMove)

}
