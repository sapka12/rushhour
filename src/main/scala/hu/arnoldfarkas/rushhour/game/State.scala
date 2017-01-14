package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game.Path._

object State {
  def isFinal(state: State, car: Car): Boolean =
    state.cars.contains(car)
}

case class State(val field: Field, val cars: Set[Car]) {

  def validMoves: Set[(Move, State)] = {
    val allValidSteps = for {
      car <- cars
      move <- car.validMoves
    } yield move

    allValidSteps.filter(_.car.positions.forall(field))
      .map(move => (move, goBy(move)))
      .filterNot(_._2 == None)
      .map(tuple => (tuple._1, tuple._2.get))
  }

  def goBy(move: Move): Option[State] = {
    val car = move.car
    val steps = car.validSteps
    if (steps.contains(move.step)) {
      car goBy move.step match {
        case Some(c) => {
          val newCars: Set[Car] =
            cars.filter(_.sign == car.sign) + c
          Some(State(field, newCars))
        }
        case None => None
      }
    } else {
      None
    }
  }
}
