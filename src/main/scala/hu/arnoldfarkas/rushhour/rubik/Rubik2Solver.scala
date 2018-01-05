package hu.arnoldfarkas.rushhour.rubik

import hu.arnoldfarkas.rushhour.game.GameSolver
import hu.arnoldfarkas.rushhour.RubikPage

case class Rubik2State(tiles: Seq[Char]) {
  assert(tiles.size == 24)

  override def toString: String = tiles.mkString

  lazy val pages: Seq[Seq[Char]] = Seq(
    tiles.drop(0).take(4),
    tiles.drop(4).take(4),
    tiles.drop(8).take(4),
    tiles.drop(12).take(4),
    tiles.drop(16).take(4),
    tiles.drop(20).take(4)
  )

  lazy val isReady = pages.forall(ts => ts.forall(t => t == ts.head))

  private lazy val permutation: Rubik2Move => Seq[Int] = _ match {
    case Rubik2Move(CW, X) =>
      Seq(15, 1, 13, 3, 0, 5, 2, 7, 8, 9, 10, 11, 12, 22, 14, 20, 18, 16, 19, 17, 4, 21, 6, 23)
    case Rubik2Move(CCW, X) =>
      Seq(4, 1, 6, 3, 20, 5, 22, 7, 8, 9, 10, 11, 12, 2, 14, 0, 17, 19, 16, 18, 15, 21, 13, 23)

    case Rubik2Move(CW, Y) =>
      Seq(0, 1, 2, 3, 4, 5, 18, 19, 8, 9, 6, 7, 12, 13, 10, 11, 16, 17, 14, 15, 22, 20, 23, 21)
    case Rubik2Move(CCW, Y) =>
      Seq(0, 1, 2, 3, 4, 5, 10, 11, 8, 9, 14, 15, 12, 13, 18, 19, 16, 17, 6, 7, 21, 23, 20, 22)

    case Rubik2Move(CW, Z) =>
      Seq(0, 1, 19, 17, 6, 4, 7, 5, 2, 9, 3, 11, 12, 13, 14, 15, 16, 20, 18, 21, 10, 8, 22, 23)
    case Rubik2Move(CCW, Z) =>
      Seq(0, 1, 8, 10, 5, 7, 4, 6, 21, 9, 20, 11, 12, 13, 14, 15, 16, 3, 18, 2, 17, 19, 22, 23)
  }

  def via(move: Rubik2Move) = Rubik2State(tiles.indices.map(i => tiles(permutation(move)(i))))
}


sealed trait Rotation

case object CW extends Rotation

case object CCW extends Rotation

sealed trait Dimension

case object X extends Dimension

case object Y extends Dimension

case object Z extends Dimension

case class Rubik2Move(rotation: Rotation, dimension: Dimension)

class Rubik2Solver extends GameSolver[Rubik2State, Rubik2Move] {
  override val actions = for {
    dim <- Set(X, Y, Z)
    rot <- Set(CW, CCW)
  } yield Rubik2Move(rot, dim)

  override def step(gameState: Rubik2State, move: Rubik2Move): Option[Rubik2State] = Some(gameState via move)

  override def isFinal(gameState: Rubik2State) = gameState.isReady
}

object Demo extends App {
  val cube = "yyobbrroyowgbrwggwybwgor"

  val state = Rubik2State(cube.toCharArray.toSeq)
  val solver = new Rubik2Solver
  val solution = solver.solve(state)

  solution.foreach(println)
}
