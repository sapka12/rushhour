package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game._

object RushHourState {
  def isFinal(state: RushHourState, car: Car): Boolean =
    state.cars.contains(car)
}

case class RushHourState(val cars: Set[Car])(implicit val field: Field) {

  override def toString: String = "State: "+ cars.toString()

  override def equals(obj: scala.Any): Boolean = obj match {
    case s: RushHourState => /*s.field == field &&*/ s.cars == cars
    case _ => false
  }


  def isValid: Boolean = {
    val allPos = cars.toList.flatMap(_.positions)
    allPos.toSet.size == allPos.size
  }

}
