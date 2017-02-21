package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._
import org.scalatest.FlatSpec

class CarSpec extends FlatSpec {
  behavior of "equals"

  it should "equls" in {
    val aCar = Car(Set(Pos(0, 2), Pos(0, 3)), 'a')
    val bCar = Car(Set(Pos(0, 3), Pos(0, 2)), 'a')

    assert(aCar == bCar)
  }

  behavior of "validMoves"

  it should "go up or down if it is vertical" in {
    val vertiCar = Car(Set(Pos(0, 2), Pos(0, 3)), 'a')
    val expected = Set(
      Move(Car(Set(Pos(0, 1), Pos(0, 2)), 'a'), Up),
      Move(Car(Set(Pos(0, 3), Pos(0, 4)), 'a'), Down)
    )
    val actual = vertiCar.validMoves

    assertResult(expected)(actual)
  }

  it should "go left or right if it is vertical" in {
    val horCar = Car(Set(Pos(3, 2), Pos(2, 2)), 'a')
    val expected = Set(
      Move(Car(Set(Pos(2, 2), Pos(1, 2)), 'a'), Left),
      Move(Car(Set(Pos(3, 2), Pos(4, 2)), 'a'), Right)
    )
    val actual = horCar.validMoves

    assertResult(expected)(actual)
  }
}
