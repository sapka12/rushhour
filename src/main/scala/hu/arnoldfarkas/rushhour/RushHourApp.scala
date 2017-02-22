package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._
import hu.arnoldfarkas.rushhour.game.GameTree._

object RushHourApp extends App {
  val begin = System.currentTimeMillis()

  val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'a')
  val startState: State = GameFactory.state(
    """xxxxxb
      |xxxxxb
      |xxaaxb
      |cxdddx
      |cxxfee
      |xxxfxx
    """.stripMargin
    , "abcdef".toSet, 'x')

  //  val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'd')
  //  val startState: State = GameFactory.state(
  //    """xabbcc
  //      |xaxxxe
  //      |xaxdde
  //      |xfxhii
  //      |xfxhjk
  //      |xgggjk
  //    """.stripMargin
  //    , "abcdefghijk".toSet, 'x')

  //      val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'p')
  //      val startState: State = GameFactory.state(
  //        """xxxxsx
  //          |xxxxsx
  //          |ppxxsx
  //          |xxxxzz
  //          |xnxrrr
  //          |xnxkkk
  //        """.stripMargin
  //        , "pszrkn".toSet, 'x')

  //  val finalCarPosition: Car = Car(Set(Pos(2, 2), Pos(3, 2)), 'a')
  //  val startState: State = GameFactory.state(
  //    """xxxx
  //      |xccc
  //      |aaxb
  //      |xxxb
  //    """.stripMargin
  //    , "abc".toSet, 'x')

  println(solve(startState, finalCarPosition).headOption)

  val end = System.currentTimeMillis()
  println(end - begin)
}
