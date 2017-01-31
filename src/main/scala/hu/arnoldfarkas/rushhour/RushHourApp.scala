package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._

object RushHourApp {

  def main(args: Array[String]): Unit = {

    val finalCarPosition: Car = Car(Set(Pos(4, 2), Pos(5, 2)), 'd')
    val startState: State = GameFactory.state(
      """xabbcc
        |xaxxxe
        |xaxdde
        |xfxhii
        |xfxhjk
        |xgggjk
      """.stripMargin
      , "abcdefghijk".toSet, 'x')

//    val finalCarPosition: Car = Car(Set(Pos(2, 2), Pos(3, 2)), 'a')
//    val startState: State = GameFactory.state(
//      """xxxx
//        |xccc
//        |aaxb
//        |xxxb
//      """.stripMargin
//      , "abc".toSet, 'x')

    val gameTree = GameTree.build(startState)

    gameTree.histories.map(_.path).foreach(println)

    val solution: Path = gameTree.solution(finalCarPosition).get

    println("solution:")
    println(solution.moves.size)
    println(solution)
  }

  def parseCar(inputCar: String): Car = {
    val split = inputCar.split(",")
    val id = split.head(0)
    val groupedPos: Set[Array[String]] = split.tail.grouped(2).toSet
    val positions = groupedPos.map(p => Pos(p(0).toInt, p(1).toInt))
    Car(positions, id)
  }
}
