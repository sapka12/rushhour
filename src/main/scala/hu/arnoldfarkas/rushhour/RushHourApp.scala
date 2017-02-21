package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._

object RushHourApp extends App {

//      val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'd')
//      val startState: State = GameFactory.state(
//        """xabbcc
//          |xaxxxe
//          |xaxdde
//          |xfxhii
//          |xfxhjk
//          |xgggjk
//        """.stripMargin
//        , "abcdefghijk".toSet, 'x')

      val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'p')
      val startState: State = GameFactory.state(
        """xxxxsx
          |xxxxsx
          |ppxxsx
          |xxxxzz
          |xnxrrr
          |xnxkkk
        """.stripMargin
        , "pszrkn".toSet, 'x')

//  val finalCarPosition: Car = Car(Set(Pos(2, 2), Pos(3, 2)), 'a')
//  val startState: State = GameFactory.state(
//    """xxxx
//      |xccc
//      |aaxb
//      |xxxb
//    """.stripMargin
//    , "abc".toSet, 'x')

  val gameTree = GameTree.build(startState)
  println(gameTree.solution(finalCarPosition).headOption)
}
