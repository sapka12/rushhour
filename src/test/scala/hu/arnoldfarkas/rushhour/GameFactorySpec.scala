package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._
import org.scalatest.FlatSpec

class GameFactorySpec extends FlatSpec {
  behavior of "GameFactory"

  it should "find all positions of a char" in {
    val input =
      """xxxxxb
        |xxxxxb
        |aaxxxb
        |xxxxcc
        |xxxxxx
        |xxxxxx
      """.stripMargin

    val expected: Set[Pos] = Set(Pos(5, 0), Pos(5, 1), Pos(5, 2))
    val actual: Set[Pos] = GameFactory.positionsForChar('b', input).positions
    assertResult(expected)(actual)
  }

  it should "create field" in {
    val input =
      """xxxxxb
        |xxxxxb
        |aaxxxb
        |xxxxcc
        |xxxxxx
        |xxxxxx
      """.stripMargin

    val field = GameFactory.createField(input, "xabc".toSet).f

    assert(field(Pos(0, 0)))
    assert(field(Pos(0, 5)))
    assert(field(Pos(5, 0)))
    assert(field(Pos(5, 5)))
    assert(field(Pos(2, 3)))

    assert(!field(Pos(-1, 0)))
    assert(!field(Pos(0, -1)))
    assert(!field(Pos(5, 6)))
    assert(!field(Pos(6, 5)))
    assert(!field(Pos(6, 6)))
  }

  it should "define final state" in {
    val finalCarPos: Car = Car(Set(Pos(0, 2), Pos(1, 2)), 'a')
    val emptySpace = 'x'

    val theFinalState: RushHourState = GameFactory.state(
      """xxxxxb
        |xxxxxb
        |aaxxxb
        |xxxxcc
        |xxxxxx
        |xxxxxx
      """.stripMargin, emptySpace)

    val notFinalState: RushHourState =
      GameFactory.state(
        """xxxxxx
          |xxxxxx
          |xxxxaa
          |xxxccb
          |xxxxxb
          |xxxxxb
      """.stripMargin, emptySpace)

    assert(RushHourState.isFinal(theFinalState, finalCarPos))
    assert(!RushHourState.isFinal(notFinalState, finalCarPos))
  }

  it should "find the cars" in {
    val input =
      """xxxxxb
        |xxxxxb
        |aaxxxb
        |xxxxcc
        |xxxxxx
        |xxxxxx
      """.stripMargin

    val cars: Set[Car] = GameFactory.cars(input, "abc".toSet)

    assert(cars.size == 3)
    assert(cars.contains(Car(Set(Pos(0, 2), Pos(1, 2)), 'a')))
    assert(cars.contains(Car(Set(Pos(5, 0), Pos(5, 1), Pos(5, 2)), 'b')))
    assert(cars.contains(Car(Set(Pos(4, 3), Pos(5, 3)), 'c')))
  }

}
