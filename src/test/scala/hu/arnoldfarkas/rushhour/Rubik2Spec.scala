package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.rubik.{Rubik2Solver, Rubik2State}
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

  it should "solve in 1 step" in {
    val s1 = Rubik2State("brbrobobwwwwgrgryyyygogo".toCharArray.toSeq)

    val solver = new Rubik2Solver()
    val solution = solver.solve(s1)

    val s = solution.get
    s.size shouldBe 1

  }

  it should "solve in 2 step" in {
    val s1 = Rubik2State("yrybgyoyobobwbwrrrggwgwo".toCharArray.toSeq)

    val solver = new Rubik2Solver()
    val solution = solver.solve(s1)

    val s = solution.get
    s.size shouldBe 2
    println(s)
  }

  it should "solve in 5 step" in {

    val start = System.currentTimeMillis

    val s1 = Rubik2State("yrgorybogywrbowgbywowgrb".toCharArray.toSeq)

    val solver = new Rubik2Solver()
    val solution = solver.solve(s1)

    val s = solution.get
    s.size shouldBe 5
    println(s)

    val end = System.currentTimeMillis
    println(end - start)
  }


  it should "solve in 8 step" in {

    val start = System.currentTimeMillis

    val s1 = Rubik2State("byogwygyrbowrwoorgyrwgbb".toCharArray.toSeq)

    val solver = new Rubik2Solver()
    val solution = solver.solve(s1)

    val s = solution.get
    s.size shouldBe 8
    println(s)

    val end = System.currentTimeMillis
    println(end - start)
  }


//  it should "solve in 10 step" in {
//
//    val start = System.currentTimeMillis
//
//    val s1 = Rubik2State("gbyybgbrorgwyyowrogwrwob".toCharArray.toSeq)
//
//    val solver = new Rubik2Solver()
//    val solution = solver.solve(s1)
//
//    val s = solution.get
//    s.size shouldBe 10
//    println(s)
//
//    val end = System.currentTimeMillis
//    println(end - start)
//  }

}
