package hu.arnoldfarkas.rushhour.rushhour

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

}
