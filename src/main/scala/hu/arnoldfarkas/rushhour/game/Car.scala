package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game._

case class Car(val positions: Set[Pos], val sign: Char) {

  def validMoves: Set[RushHourMove] = {

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

    validSteps.map(step => goBy(step) match {
      case Some(car) => Some(RushHourMove(car, step))
      case None => None
    }).flatten
  }

  private def toOrderedList(ps: Set[Pos]): List[Pos] = ps.toList.sortWith{
    case (Pos(x1, y1), Pos(x2, y2)) =>
      if(x1 == x2) y1 < y2
      else x1 < x2
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case Car(cPos, cSign) => cSign == sign &&
      toOrderedList(cPos) == toOrderedList(positions)
    case _ => false
  }
}