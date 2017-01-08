package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.Game._

import scala.io.Source

object RushHourApp {

  def main(args: Array[String]): Unit = {
    val inputFilename = args(0)
    val finalCarPosition: Car = parseCar(args(1)) // a,0,2,1,2

    val finalState: FinalState = GameFactory.finalState(finalCarPosition)
    val file = Source.fromFile(inputFilename)
    val gameState: State = GameFactory.state(file.mkString, "abc".toSet, 'x')

    val gameTree =

    println(gameState)
  }

  def parseCar(inputCar: String): Car = {
    val split = inputCar.split(",")
    val id = split.head(0)
    val groupedPos: Set[Array[String]] = split.tail.grouped(2).toSet
    val positions = groupedPos.map(p => Pos(p(0).toInt, p(1).toInt))
    (positions, id)
  }
}
