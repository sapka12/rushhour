package hu.arnoldfarkas.rushhour.game

object Path {
  type Field = Pos => Boolean

  sealed trait Step
  case object Up extends Step
  case object Down extends Step
  case object Left extends Step
  case object Right extends Step

  case class Move(val car: Car, val step: Step) {
    //    override def toString: String =
    //      "[" + car.sign.toString + {
    //        step match {
    //          case Up => "^"
    //          case Down => "v"
    //          case Left => "<"
    //          case Right => ">"
    //        }
    //      } + "]"
  }

  type Path = List[Move]
}
