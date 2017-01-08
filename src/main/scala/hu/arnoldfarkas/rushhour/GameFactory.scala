package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.Game._

object GameFactory {

  def finalState(finalCarPosition: (Set[Pos], Char)): FinalState =
    (state, car) => state._2.contains(car)

  private def toMatrix(input: String) =
    input.split("\n").toList.map(_.toList)

  def createField(input: String, fieldChars: Set[Char]): Field =
    pos => fieldChars.flatMap(positionsForChar(_, input)).contains(pos)

  def positionsForChar(c: Char, input: String): Set[Pos] = {

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

    findCharInArr(c, toMatrix(input), 0, Set())
  }

  def cars(input: String, carIds: Set[Char]): Set[Car] =
    carIds.map(carId => (positionsForChar(carId, input), carId))

  def state(inputMap: String, carIds: Set[Char], emptyPlaceId: Char): State =
    (createField(inputMap, carIds + emptyPlaceId), cars(inputMap, carIds))
}
