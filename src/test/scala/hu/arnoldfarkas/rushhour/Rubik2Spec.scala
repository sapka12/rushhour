package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.rubik.Rubik2State
import org.scalatest.{FlatSpec, Matchers}

class Rubik2Spec  extends FlatSpec with Matchers {

  behavior of "Rubik2State"

  it should "be found by equals" in {

    val s1 = Rubik2State("yyobbrroyowgbrwggwybwgor".toCharArray.toSeq)
    val s2 = Rubik2State("yoybbrroyowgbrwggwybwgor".toCharArray.toSeq)
    val s2b = Rubik2State("yoybbrroyowgbrwggwybwgor".toCharArray.toSeq)
    val s3 = Rubik2State("yoybbrroyowgbrwggwybwgro".toCharArray.toSeq)

    val set = Set(s1, s2)

    set should contain(s1)
    set should contain(s2)
    set should contain(s2b)

    set shouldNot contain(s3)
  }
}
