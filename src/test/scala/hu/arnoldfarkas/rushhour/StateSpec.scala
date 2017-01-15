package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game.{Car, Path, Pos, State}
import org.scalatest.FlatSpec
import hu.arnoldfarkas.rushhour.game.Path.{Move, sqField}

class StateSpec extends FlatSpec {
  behavior of "validMoves"

  it should "return only the valid steps" in {
    val field = sqField(6)

    val a = Car(Set(Pos(0, 2), Pos(1, 2)), 'a')
    val b = Car(Set(Pos(5, 0), Pos(5, 2), Pos(5, 1)), 'b')
    val c = Car(Set(Pos(5, 3), Pos(4, 3)), 'c')

    val state = State(field, Set(a, b, c))

    val actual: Set[(Move, State)] = state.validMoves
    val expected: Set[(Move, State)] = Set(
      (Move(a, Path.Right),
        State(field,
          Set(
            a.copy(positions = Set(Pos(2, 2), Pos(1, 2))),
            b,
            c
          ))),
      (Move(c, Path.Left),
        State(field,
          Set(
            a,
            b,
            c.copy(positions = Set(Pos(3, 3), Pos(4, 3)))
          )))
    )

    if (expected.size != actual.size) actual.foreach(println)

    assertResult(expected.size)(actual.size)
    assertResult(expected)(actual)
  }

}
