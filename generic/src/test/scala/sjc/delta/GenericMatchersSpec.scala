package sjc.delta

import org.scalatest.{Matchers, FreeSpec}
import org.scalatest.exceptions.TestFailedException
import shapeless.HNil

import sjc.delta.matchers.{beDifferentTo, beIdenticalTo}
import sjc.delta.generic.GenericDelta._
import sjc.delta.std.int._
import sjc.delta.Delta.DeltaOps

//import sjc.delta.matchers.syntax.anyDeltaMatcherOps

class GenericMatchersSpec extends FreeSpec with Matchers {
  "generic" - {
    "beDifferentTo" in {
      bob should beDifferentTo(sue)

      intercept[TestFailedException] {
        bob should beDifferentTo(bob)
      }.message shouldBe Some("No differences detected")
    }
  }

  "not beDifferentTo" ignore {
    bob should not(beDifferentTo(bob))

    intercept[TestFailedException] {
      bob should not(beDifferentTo(sue))
    }.message shouldBe Some(
      """Detected the following differences:
        |  Person(
        |-   age = 11,
        |+   age = 22,
        |-   name = "bob"
        |+   name = "sue"
        |    dog = Dog(
        |-     age = 1,
        |+     age = 2,
        |      name = "fido"
        |    )
        |  )
        |  """.stripMargin
    )
  }


  implicit val deltaPatch: Patch[(String, String), Unit] = sjc.delta.Delta.fallback.fallbackDelta[String]
  implicit val deltaString: Delta.Aux[String, (String, String)] = sjc.delta.Delta.fallback.fallbackDelta[String]

  private val fido: Dog = Dog(1, "fido")
  private val fifi: Dog = Dog(2, "fido")
  private val bob = Person(11, "bob", fido)
  private val sue = Person(22, "sue", fifi)

  case class Person(age: Int, name: String, pet: Dog)
  case class Dog(age: Int, name: String)
}
