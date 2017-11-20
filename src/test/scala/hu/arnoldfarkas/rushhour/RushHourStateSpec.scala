package hu.arnoldfarkas.rushhour

import org.scalatest.FlatSpec
import hu.arnoldfarkas.rushhour.game._
import Field._

class RushHourStateSpec extends FlatSpec {

  def sqField(size: Int)(pos: Pos): Boolean =
    pos.x >= 0 && pos.x < size && pos.y >= 0 && pos.y < size

  "State" should "equals" in {
    implicit val f: Field = Field(_ => true)

    val aCar = Car(Set(Pos(0, 2), Pos(0, 3)), 'a')
    val aCar2 = Car(Set(Pos(0, 3), Pos(0, 2)), 'a')
    val bCar = Car(Set(Pos(2, 3), Pos(3, 3)), 'a')
    val bCar2 = Car(Set(Pos(3, 3), Pos(2, 3)), 'a')

    val s1 = RushHourState(Set(aCar, bCar))
    val s2 = RushHourState(Set(aCar2, bCar2))

    assert(s1 == s2)
  }

  behavior of "validMoves"

  it should "return only the valid steps : small" in {
    implicit val field = Field(sqField(3))

    val a = Car(Set(Pos(0, 0), Pos(0, 1)), 'a')
    val b = Car(Set(Pos(0, 2), Pos(1, 2)), 'b')

    val state = RushHourState(Set(a, b))

    val b_v2 = b.copy(positions = Set(Pos(2, 2), Pos(1, 2)))

    val actual: Set[(Car, RushHourMove, RushHourState)] = state.validMoves
    val expected: Set[(Car, RushHourMove, RushHourState)] = Set(
      (
        b,
        RushHourMove(b_v2, Right),
        RushHourState(
          Set(
            b_v2,
            a
          )))
    )

    assertResult(expected.size)(actual.size)
    assertResult(expected)(actual)
  }

  it should "return only the valid steps" in {
    implicit val field = Field(sqField(6))

    val a = Car(Set(Pos(0, 2), Pos(1, 2)), 'a')
    val b = Car(Set(Pos(5, 0), Pos(5, 2), Pos(5, 1)), 'b')
    val c = Car(Set(Pos(5, 3), Pos(4, 3)), 'c')

    val state = RushHourState(Set(a, b, c))

    val a_v2 = a.copy(positions = Set(Pos(2, 2), Pos(1, 2)))
    val c_v2 = c.copy(positions = Set(Pos(3, 3), Pos(4, 3)))

    val actual: Set[(Car, RushHourMove, RushHourState)] = state.validMoves
    val expected: Set[(Car, RushHourMove, RushHourState)] = Set(
      (
        a,
        RushHourMove(a_v2, Right),
        RushHourState(Set(a_v2,b,c))
      ),(
        c,
        RushHourMove(c_v2, Left),
        RushHourState(Set(a,b,c_v2))
      )
    )

    assertResult(expected.size)(actual.size)
    assertResult(expected)(actual)
  }

}
