package hu.arnoldfarkas.rushhour.rushhour

import hu.arnoldfarkas.rushhour.rushhour

case class Car(val positions: Set[Pos], val sign: Char) {

  def hasCommonPos(c: Car): Boolean = !c.positions.forall(!positions.contains(_))

  def inField(f: Field) = positions.forall(f.f)

  private lazy val isHorizontal = {
    val headY = positions.toList.head.y
    positions.forall(_.y == headY)
  }

  def validStep(step: Step) =
    (isHorizontal && (step == rushhour.Left || step == rushhour.Right)) ||
      (!isHorizontal && (step == Up || step == Down))
}