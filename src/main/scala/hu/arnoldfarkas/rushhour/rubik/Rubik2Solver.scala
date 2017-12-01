package hu.arnoldfarkas.rushhour.rubik

import hu.arnoldfarkas.rushhour.game.GameSolver
import hu.arnoldfarkas.rushhour.RubikPage


case class Rubik2State(
                        x0: RubikPage,
                        x1: RubikPage,
                        y0: RubikPage,
                        y1: RubikPage,
                        z0: RubikPage,
                        z1: RubikPage
                      ) {
  lazy val pages: Seq[RubikPage] = Seq(x0, y0, z0, x1, y1, z1)
  lazy val isReady = pages.forall(_.isReady)

  def via(move: Rubik2Move): Rubik2State = move match {
    case Rubik2Move(CW, X) => ???
    case Rubik2Move(CW, Y) => ???
    case Rubik2Move(CW, Z) => ???
    case Rubik2Move(CCW, X) => ???
    case Rubik2Move(CCW, Y) => ???
    case Rubik2Move(CCW, Z) => ???
  }
}


sealed trait Rotation
case object CW extends Rotation
case object CCW extends Rotation

sealed trait Dimension
case object X extends Dimension
case object Y extends Dimension
case object Z extends Dimension

case class Rubik2Move(rotation: Rotation, dimension: Dimension)

object Rubik2Solver {
}

class Rubik2Solver extends GameSolver[Rubik2State, Rubik2Move]{
  override val actions = for {
    dim <- Set(X, Y, Z)
    rot <- Set(CW, CCW)
  } yield Rubik2Move(rot, dim)

  override def step(gameState: Rubik2State, move: Rubik2Move): Rubik2State = gameState via move

  override def isFinal(gameState: Rubik2State) = gameState.isReady
}
