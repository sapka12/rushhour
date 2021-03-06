package hu.arnoldfarkas.rushhour.rushhour

case class Field(f: Pos => Boolean)

sealed trait Step
case object Up extends Step
case object Down extends Step
case object Left extends Step
case object Right extends Step

case class RushHourMove(val carSign: Char, val step: Step) {
  override def toString: String =
    carSign.toString + {
      step match {
        case Up => "^"
        case Down => "v"
        case Left => "<"
        case Right => ">"
      }
    }
}
