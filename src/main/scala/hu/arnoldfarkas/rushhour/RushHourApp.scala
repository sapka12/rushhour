package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game._

object RushHourApp {

  def main(args: Array[String]): Unit = {

    val finalCarPosition: Car = Car(Set(Pos(2, 1), Pos(3, 1)), 'a')
    val startState: State = GameFactory.state(
      """xxxb
        |aaxb
        |xxxx
        |xxxx
      """.stripMargin
      , "ab".toSet, 'x')

    val gameTree = GameTree.build(startState)

    val solution: Path = gameTree.histories.filter(p => State.isFinal(p.state, finalCarPosition)).head.path

    println("solution:")
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
