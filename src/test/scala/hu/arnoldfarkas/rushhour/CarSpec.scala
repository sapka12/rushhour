package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game.Path._
import hu.arnoldfarkas.rushhour.game.{Car, Pos}
import org.scalatest.FlatSpec

class CarSpec extends FlatSpec {
  behavior of "Car"

  it should "go up or down if it is vertical" in {
    val vertiCar = Car(Set(Pos(0, 2), Pos(0, 3)), 'a')
    val upCar = Car(Set(Pos(0, 1), Pos(0, 2)), 'a')
    val downCar = Car(Set(Pos(0, 3), Pos(0, 4)), 'a')

    assertResult(Some(upCar))(vertiCar.goBy(Up))
    assertResult(Some(downCar))(vertiCar.goBy(Down))
    assertResult(None)(vertiCar.goBy(Left))
    assertResult(None)(vertiCar.goBy(Right))
  }

  it should "go left or right if it is horizontal" in {
    val vertiCar = Car(Set(Pos(3, 2), Pos(4, 2), Pos(2, 2)), 'a')
    val leftCar = Car(Set(Pos(2, 2), Pos(3, 2), Pos(1, 2)), 'a')
    val rightCar = Car(Set(Pos(4, 2), Pos(5, 2), Pos(3, 2)), 'a')

    assertResult(None)(vertiCar.goBy(Up))
    assertResult(None)(vertiCar.goBy(Down))
    assertResult(Some(leftCar))(vertiCar.goBy(Left))
    assertResult(Some(rightCar))(vertiCar.goBy(Right))
  }
}
