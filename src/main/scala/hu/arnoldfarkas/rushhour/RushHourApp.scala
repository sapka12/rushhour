package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.game.Path._
import hu.arnoldfarkas.rushhour.game._

import scala.io.Source

object RushHourApp {



  def main(args: Array[String]): Unit = {
//    val inputFilename = args(0)
//    val finalCarPosition: Car = parseCar(args(1)) // a,0,2,1,2
//
//    val file = Source.fromFile(inputFilename)
//
//    val startState: State = GameFactory.state(file.mkString, "abc".toSet, 'x')

    val finalCarPosition: Car = Car(Set(Pos(2, 1), Pos(3, 1)), 'a')
    val startState: State = GameFactory.state(
      """xxxb
        |aaxb
        |xxxx
        |xxxx
      """.stripMargin
      , "ab".toSet, 'x')

    val gameTree = GameTree.build(startState)

    val solution: Path = gameTree.filter(p => State.isFinal(p._1, finalCarPosition)).head._2

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
