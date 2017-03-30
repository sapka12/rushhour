package hu.arnoldfarkas.rushhour.generator

object Generator extends App {

  println("--- started ---")

  def generate[T](values: List[T], length: Int): Stream[List[T]] = {

    def init(e: T) =
      (for {
        _ <- 1 to length
      } yield e).toList

    val originalHead = values.head
    val originalLast = values.reverse.head

    val initElement = init(originalHead)
    val endElement = init(originalLast)

    def stream: Stream[List[T]] = {

      def streamBuilder(n: List[T]): Stream[List[T]] = {
        if (n == endElement) Stream(n)
        else n #:: streamBuilder(inc(n).toList)
      }

      def inc(elem: Seq[T]): Seq[T] = {
        val asNum = elem.map(e => values.indexOf(e)).mkString
        val next = BigInt.apply(asNum, values.size) + 1
        val value = next.toString(values.size)
        val diff = length - value.size
        ((for (_ <- 1 to diff) yield "0").mkString + value).map(i =>
          values(i.toString.toInt))
      }

      streamBuilder(initElement)
    }

    stream
  }

  val values = List(0, 1, 2, 3)

  val length = 3

  val start = System.currentTimeMillis()

  val result = generate(values, length).toList.map(_.mkString)

  val end = System.currentTimeMillis()

  println(result.mkString("\n"))
  println(end - start)
}
