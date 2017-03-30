package hu.arnoldfarkas.rushhour.generator

object Generator extends App {

  println("--- started ---")

  def generate[T](values: List[T], length: Int): List[List[T]] = {

    def init(e: T) =
      (for {
        _ <- 1 to length
      } yield e).toList

    val originalHead = values.head
    val originalLast = values.reverse.head

    val initElement = init(originalHead)
    val endElement = init(originalLast)

    def inc(elem: Seq[T]): Seq[T] = {

      def incIdx(elem: Seq[T], idx: Int): Seq[T] = {
        if (elem(idx) == originalLast) {
          val e: Seq[T] = elem.patch(idx, Seq(originalHead), 1)
          incIdx(e, idx + 1)
        } else {
          val actualVal = values.indexOf(elem(idx))
          val incVal = values(actualVal + 1)
          elem.patch(idx, Seq(incVal), 1)
        }
      }

      incIdx(elem, 0)
    }

    def addNextElement(lastElem: List[T], aggr: List[List[T]]): List[List[T]] = {
      if (lastElem == endElement) lastElem :: aggr
      else {
        addNextElement(inc(lastElem).toList, lastElem :: aggr)
      }
    }

    addNextElement(initElement, List())
  }

  val values = List(0, 1, 2, 3, 5)

  val length = 11

  val start = System.currentTimeMillis()

  val result = generate(values, length).map(_.mkString)

  val end = System.currentTimeMillis()

  println(result.mkString(", "))

  println(end - start)

}
