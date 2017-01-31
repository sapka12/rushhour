package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._
import org.scalatest.FlatSpec

class GameTreeSpec extends FlatSpec {

  behavior of "tree"

  it should "end" in {
    def tree[A](visited: List[A], next: List[A] => Option[A]): List[A] = {
      next(visited) match {
        case None => visited
        case Some(elem) => tree(elem :: visited, next)
      }
    }

    val integers = List(5)
    def method(ints: List[Int]): Option[Int] =
      if (ints.min > 1) {
        val next = ints.min - 1
        println(next)
        Some(next)
      }
      else {
        println("none")
        None
      }

    def none(i: List[Int]) = None

    assertResult(List(5))(tree(integers, none))
    assertResult(List(1,2,3,4,5))(tree(integers, method))
  }

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

    val actual = GameTree.nextHistory(histories).get

    assertResult(GameFactory.state(
      """xxxb
        |xaab
      """.stripMargin
      , "ab".toSet, 'x'))(actual.state)

    val expected: Path = Path(
      List(
        Move(
          Car(Set(Pos(1, 1), Pos(2, 1)), 'a'),
          Right
        )
      )
    )

    assertResult(expected)(actual.path)
  }

  it should "next" in {
    val startState: State = GameFactory.state(
      """xxxb
        |aaxb
      """.stripMargin
      , "ab".toSet, 'x')

    val historiesStep0: List[History] = List(History(startState, Path.empty))

    val history1 = GameTree.nextHistory(historiesStep0).get

    val historiesStep1: List[History] = List(History(startState, Path.empty), history1)

    assertResult(None)(GameTree.nextHistory(historiesStep1))
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


  it should "have 4 paths" in {
    def createState(str: String): State = GameFactory.state(
      str, "ab".toSet, 'x')

    val startState =
      """xxxx
        |aaxb
        |xxxb
      """.stripMargin


    val possibleStates: Set[State] = Set(
      startState,
      """xxxb
        |aaxb
        |xxxx
      """.stripMargin,
      """xxxb
        |xaab
        |xxxx
      """.stripMargin,
      """xxxa
        |xaab
        |xxxb
      """.stripMargin
    ).map(createState)

    val tree = GameTree.build(createState(startState))

    tree.histories.map(_.path).foreach(println)

    assertResult(possibleStates.size)(tree.histories.size)

    tree.histories.map(_.state).foreach(st =>
      assert(possibleStates.contains(st))
    )
  }

}
