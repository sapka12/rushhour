package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game.Path._

case class Car(val positions: Set[Pos], val sign: Char) {

  def validMoves: Set[Move] = positions match {
    case positions if positions.map(_.x).forall(_ == positions.head.x) => {
      Set(
        Move(Car(positions.map(pos => Pos(pos.x, pos.y - 1)), sign), Up),
        Move(Car(positions.map(pos => Pos(pos.x, pos.y + 1)), sign), Down)
      )
    }
    case positions if positions.map(_.y).forall(_ == positions.head.y) => {
      Set(
        Move(Car(positions.map(pos => Pos(pos.x - 1, pos.y)), sign),
          Left),
        Move(Car(positions.map(pos => Pos(pos.x + 1, pos.y)), sign),
          Right)
      )
    }
  }

  def validSteps: Set[Step] = {
    val pos = this.positions.toList(0)
    if (this.positions.forall(_.x == pos.x)) {
      Set(Up, Down)
    } else {
      Set(Left, Right)
    }
  }

  def goBy(step: Step): Option[Car] = {
    def reposition(fp: Pos => Pos): Car = Car(positions.map(fp), sign)

    if (validSteps.contains(step)) {
      Some(step match {
        case Up => reposition(p => Pos(p.x, p.y - 1))
        case Down => reposition(p => Pos(p.x, p.y + 1))
        case Left => reposition(p => Pos(p.x - 1, p.y))
        case Right => reposition(p => Pos(p.x + 1, p.y))
      })
    } else {
      None
    }
  }
}