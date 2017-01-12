package hu.arnoldfarkas.rushhour

object Game {
  case class Pos(x: Int, y: Int)

  type Field = Pos => Boolean

  type Car = (Set[Pos], Char)

  type State = (Field, Set[Car])

  type FinalState = (State, Car) => Boolean

  sealed trait Step
  case object Up extends Step
  case object Down extends Step
  case object Left extends Step
  case object Right extends Step

  type Move = (Car, Step)
  type Path = List[Move]
//  def possibleSteps(state: State): Set[] {
//
//  }
//
//  type History =

}
