package hu.arnoldfarkas.rushhour

object Game {

  sealed trait Direction
  object Left extends Direction
  object Right extends Direction
  object Up extends Direction
  object Down extends Direction

  abstract class State(cars: Set[Car]){
    val field: Pos => Boolean = ???
    def gameTree: GameTree = ???
  }

  case class Pos(x: Int, y: Int)
  case class Car(id: Char, positions: Set[Pos])
  case class Step(car: Car, direction: Direction) {
    def change(state: State): State = ???
  }

  type Solution = List[Step]
  type FinalPosition = Car
  type GameTree = Stream[List[Step]]

  def solve(state: State, finalPosition: FinalPosition): Option[Solution] = ???
}
