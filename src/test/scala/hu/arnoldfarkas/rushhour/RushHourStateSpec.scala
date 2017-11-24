package hu.arnoldfarkas.rushhour

import org.scalatest.FlatSpec
import hu.arnoldfarkas.rushhour.rushhour._

class RushHourStateSpec extends FlatSpec {

  def sqField(size: Int)(pos: Pos): Boolean =
    pos.x >= 0 && pos.x < size && pos.y >= 0 && pos.y < size

  "State" should "equals" in {
    implicit val f: Field = Field(_ => true)

    val aCar = Car(Set(Pos(0, 2), Pos(0, 3)), 'a')
    val aCar2 = Car(Set(Pos(0, 3), Pos(0, 2)), 'a')
    val bCar = Car(Set(Pos(2, 3), Pos(3, 3)), 'a')
    val bCar2 = Car(Set(Pos(3, 3), Pos(2, 3)), 'a')

    val s1 = RushHourState(Set(aCar, bCar))
    val s2 = RushHourState(Set(aCar2, bCar2))

    assert(s1 == s2)
  }

}
