package hu.arnoldfarkas.rushhour

object Game {
  case class Pos(x: Int, y: Int)

  type Field = Pos => Boolean

  type Car = (Set[Pos], Char)

  type State = (Field, Set[Car])

  type SolutionPath = List[State]

  type FinalState = (State, Car) => Boolean
}
