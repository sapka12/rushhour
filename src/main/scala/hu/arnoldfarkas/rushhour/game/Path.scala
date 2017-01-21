package hu.arnoldfarkas.rushhour.game

object Path {
  val empty = Path(List.empty[Move])
}

object Field {
  def sqField(size: Int)(pos: Pos): Boolean =
    pos.x >= 0 && pos.x < size && pos.y >= 0 && pos.y < size
}

case class Field(f: Pos => Boolean)

sealed trait Step
case object Up extends Step
case object Down extends Step
case object Left extends Step
case object Right extends Step

case class Move(val car: Car, val step: Step) {
  override def toString: String =
    "[" + car.sign.toString + {
      step match {
        case Up => "^"
        case Down => "v"
        case Left => "<"
        case Right => ">"
      }
    } + "]"
}

case class Path(moves: List[Move])
case class History(state: State, path: Path)