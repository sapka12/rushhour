package hu.arnoldfarkas.rushhour

import org.scalatest.{FlatSpec, Matchers}

class RushHourMoveSpec  extends FlatSpec with Matchers {

  behavior of "RushHourMove"

  it should "have proper set eq" in {
    assert(Set(1, 2, 3) == Set(3, 2, 1))
    assert(Set(1, 2, 3, 4, 5) == Set(5, 4, 3, 2, 1))
  }

}
