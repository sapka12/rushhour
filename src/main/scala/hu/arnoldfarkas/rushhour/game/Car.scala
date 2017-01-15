package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game.Path._

case class Car(val positions: Set[Pos], val sign: Char) {

  def validMoves: Set[Move] = {
    validSteps.map(step => goBy(step) match {
      case Some(car) => Some(Move(car, step))
      case None => None
    }).flatten
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