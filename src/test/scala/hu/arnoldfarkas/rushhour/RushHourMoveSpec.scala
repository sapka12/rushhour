package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game.{Car, Pos, RushHourMove}
import org.scalatest.{FlatSpec, Matchers}

class RushHourMoveSpec  extends FlatSpec with Matchers {

  behavior of "RushHourMove"

  it should "have proper set eq" in {
    assert(Set(1, 2, 3) == Set(3, 2, 1))
    assert(Set(1, 2, 3, 4, 5) == Set(5, 4, 3, 2, 1))
  }

  it should "handle same car" in {
    import hu.arnoldfarkas.rushhour.game._

    val car = Car(Set(Pos(0, 0), Pos(0, 1)), 'a')

    RushHourMove(car, Up) shouldNot be(RushHourMove(car, Down))
    RushHourMove(car, Up) shouldNot be(RushHourMove(car, Right))
    RushHourMove(car, Up) shouldNot be(RushHourMove(car, Left))

    RushHourMove(car, Up) shouldBe(RushHourMove(car, Up))
  }

  it should "handle diff cars" in {
    import hu.arnoldfarkas.rushhour.game._

    val car = Car(Set(Pos(0, 0), Pos(0, 1)), 'a')
    val sameCar = Car(Set(Pos(0, 1), Pos(0, 0)), 'a')

    RushHourMove(car, Up) shouldBe(RushHourMove(sameCar, Up))
  }

  it should "handle diff cars with length 3" in {
    import hu.arnoldfarkas.rushhour.game._

    val car = Car(Set(Pos(0, 0), Pos(0, 2), Pos(0, 1)), 'a')
    val sameCar = Car(Set(Pos(0, 2), Pos(0, 1), Pos(0, 0)), 'a')

    RushHourMove(car, Up) shouldBe(RushHourMove(sameCar, Up))
  }

  it should "handle diff chars of cars" in {
    import hu.arnoldfarkas.rushhour.game._

    val car = Car(Set(Pos(0, 0), Pos(0, 2), Pos(0, 1)), 'a')

    RushHourMove(car, Up) shouldNot be(RushHourMove(car.copy(sign = 'b'), Up))
  }

}
