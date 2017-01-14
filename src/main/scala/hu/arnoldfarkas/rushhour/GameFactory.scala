package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.Game._

object GameFactory {


  private def toMatrix(input: String) =
    input.split("\n").toList.map(_.toList)

  def createField(input: String, fieldChars: Set[Char]): Field =
    pos => fieldChars.flatMap(positionsForChar(_, input).positions).contains(pos)

  def positionsForChar(c: Char, input: String): Car = {

    def findCharInArr(c: Char,
                      lines: List[List[Char]],
                      lineIdx: Int,
                      aggr: Set[Pos]): Set[Pos] = {

      def findCharInLine(c: Char,
                         line: List[Char],
                         idxInLine: Int,
                         aggr: Set[Pos]): Set[Pos] = {
        val idx = line.indexOf(c, idxInLine)
        if (idx < 0) aggr
        else findCharInLine(c, line, idx + 1, aggr + Pos(idx, lineIdx))
      }

      lines match {
        case List() => aggr
        case head :: tail =>
          findCharInArr(c,
                        tail,
                        lineIdx + 1,
                        findCharInLine(c, head, 0, Set()) ++ aggr)
      }
    }

    Car(findCharInArr(c, toMatrix(input), 0, Set()), c)
  }

  def cars(input: String, carIds: Set[Char]): Set[Car] =
    carIds.map(positionsForChar(_, input))

  def state(inputMap: String, carIds: Set[Char], emptyPlaceId: Char): State =
    State(createField(inputMap, carIds + emptyPlaceId), cars(inputMap, carIds))
}
