package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._
import org.scalatest.FlatSpec

class GameTreeSpec extends FlatSpec {

  behavior of "nextHistory"

  it should "no other state" in {
    val startState: State = GameFactory.state(
      """xxb
        |aab
      """.stripMargin
      , "ab".toSet, 'x')

    val histories: List[History] = List(History(startState, Path.empty))

    val actual = GameTree.nextHistory(histories)

    assertResult(None)(actual)
  }


  it should "one more state" in {
    val startState: State = GameFactory.state(
      """xxxb
        |aaxb
      """.stripMargin
      , "ab".toSet, 'x')

    val histories: List[History] = List(History(startState, Path.empty))

    val actual = GameTree.nextHistory(histories)

    assertResult(GameFactory.state(
      """xxxb
        |xaab
      """.stripMargin
      , "ab".toSet, 'x'))(actual.get.state)
  }

  behavior of "build"

  it should "no other state" in {
    val startState: State = GameFactory.state(
      """xxb
        |aab
      """.stripMargin
      , "ab".toSet, 'x')

    val expected: List[History] = List(History(startState, Path.empty))

    val actual: List[History] = GameTree.build(startState).histories

    assertResult(expected)(actual)
  }

  it should "have 2 states" in {
    val startState: State = GameFactory.state(
      """xxxb
        |aaxb
      """.stripMargin
      , "ab".toSet, 'x')

    assertResult(2)(GameTree.build(startState).histories.size)
  }

}
