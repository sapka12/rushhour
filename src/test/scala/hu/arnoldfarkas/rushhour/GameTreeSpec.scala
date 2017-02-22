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
        Some(next)
      }
      else None

    def none(i: List[Int]) = None

    assertResult(List(5))(tree(integers, none))
    assertResult(List(1,2,3,4,5))(tree(integers, method))
  }

  behavior of "build"

  it should "no other state" in {
    val startState: State = GameFactory.state(
      """xxb
        |aab
      """.stripMargin
      , "ab".toSet, 'x')

    val expected = Stream(History(startState, Path.empty))

    val actual = GameTree.build(startState)

    assertResult(expected)(actual)
  }

  it should "have 2 states" in {
    val startState: State = GameFactory.state(
      """xxxb
        |aaxb
      """.stripMargin
      , "ab".toSet, 'x')

    assertResult(2)(GameTree.build(startState).size)
  }

//  it should "build shorter paths first" in {
//    val startState: State = GameFactory.state(
//      "xaax"
//      , "a".toSet, 'x')
//
//    val paths = GameTree.build(startState).histories.map(_.path)
//
//    assert(paths.forall(_.moves.size < 2))
//  }

//  it should "have 4 paths" in {
//    def createState(str: String): State = GameFactory.state(
//      str, "ab".toSet, 'x')
//
//    val startState = createState{
//      """xxxx
//        |aaxb
//        |xxxb
//      """.stripMargin
//    }
//
//    val states = GameTree
//      .build(startState)
//      .histories.map(_.state)
//
//    assertResult(4)(states.size)
//
//    assert(states.contains(startState))
//
//    assert(states.contains(createState{
//      """xxxb
//        |aaxb
//        |xxxx
//      """.stripMargin
//    }))
//
//      assert(states.contains(createState{
//      """xxxb
//        |xaab
//        |xxxx
//      """.stripMargin
//    }))
//
//      assert(states.contains(createState{
//      """xxxx
//        |xaab
//        |xxxb
//      """.stripMargin
//    }))
//  }

}
