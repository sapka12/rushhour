package hu.arnoldfarkas.rushhour.rubik3

import hu.arnoldfarkas.rushhour.game.GameSolver
import hu.arnoldfarkas.rushhour.rubik3.Rubik3Solver._

object Rubik3Solver {
  case class RubikMove(dimension: Dimension, rotation: Rotation, layer: Int)

  sealed trait Dimension
  case object X extends Dimension
  case object Y extends Dimension
  case object Z extends Dimension

  val Dimensions = Set(X, Y, Z)

  sealed trait Rotation
  case object CW extends Rotation
  case object CCW extends Rotation

  val Rotations = Set(CW, CCW)

  case class Rubik3State()
}

class Rubik3Solver extends GameSolver[Rubik3State, RubikMove]{

  override val actions = for {
    dim <- Dimensions
    rot <- Rotations
    layer <- (1 to 3).toSet
  } yield RubikMove(dim, rot, layer)

  override def step(gameState: Rubik3State, move: RubikMove) = Some{
    ???
  }

  override def isFinal(gameState: Rubik3State) = ???
}
