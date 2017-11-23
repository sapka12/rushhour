package hu.arnoldfarkas.rushhour.game

import hu.arnoldfarkas.rushhour.game._

case class Car(val positions: Set[Pos], val sign: Char) {

  def hasCommonPos(c: Car): Boolean = !c.positions.forall(!positions.contains(_))

  def inField(f: Field) = positions.forall(f.f)

  private lazy val isHorizontal = {
    val headY = positions.toList.head.y
    positions.forall(_.y == headY)
  }

  def validStep(step: Step) =
    (isHorizontal && (step == Left || step == Right)) ||
      (!isHorizontal && (step == Up || step == Down))

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
      case Some(car) => Some(RushHourMove(car.sign, step))
      case None => None
    }).flatten
  }
}