package hu.arnoldfarkas.rushhour.generator

import org.scalatest.{FlatSpec, Matchers}

class GeneratorSpec extends FlatSpec with Matchers {

  "generator" should "1" in {

    val generated = Generator.generate(List(0, 1, 2), 2)

    val expected = List(
      List(0, 0),
      List(0, 1),
      List(0, 2),
      List(1, 0),
      List(1, 1),
      List(1, 2),
      List(2, 0),
      List(2, 1),
      List(2, 2)
    )

    val generatedSet = generated.toSet
    val expectedSet = expected.toSet

    generatedSet shouldBe expectedSet

  }

  "generator" should "2" in {

    val generated = Generator.generate(List("0", "1"), 3)

    generated.toSet shouldBe List(
      List("0", "0", "0"),
      List("0", "0", "1"),
      List("0", "1", "0"),
      List("0", "1", "1"),
      List("1", "0", "0"),
      List("1", "0", "1"),
      List("1", "1", "0"),
      List("1", "1", "1")
    ).toSet

  }

}
