package sjc.delta.argonaut

import argonaut.Json.jString
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{FreeSpec, Matchers}
import sjc.delta.argonaut.matchers.{beDifferentTo, beIdenticalTo}


class JsonMatchersTest extends FreeSpec with Matchers with JsonTestUtil {
  "json" - {
    "beDifferentTo" in {
      jString("def") should beDifferentTo(jString("abc"))

      intercept[TestFailedException] {
        jString("def") should beDifferentTo(jString("def"))
      }.message shouldBe Some(""""def" was no different to "def"""")
    }

    "not beDifferentTo" in {
      jString("def") should not(beDifferentTo(jString("def")))

      intercept[TestFailedException] {
        jString("def") should not(beDifferentTo(jString("abc")))
      }.message shouldBe Some(
        """"def" had the following differences with "abc":
          |  {
          |    "" : {
          |      "actual" : "def",
          |      "expected" : "abc"
          |    }
          |  }""".stripMargin)
    }

    "beIdenticalTo" in {
      jString("def") should beIdenticalTo(jString("def"))

      intercept[TestFailedException] {
        jString("def") should beIdenticalTo(jString("abc"))
      }.message shouldBe Some(
        """"def" had the following differences with "abc":
          |  {
          |    "" : {
          |      "actual" : "def",
          |      "expected" : "abc"
          |    }
          |  }""".stripMargin)
    }

    "not beIdenticalTo" in {
      jString("def") should not(beIdenticalTo(jString("abc")))

      intercept[TestFailedException] {
        jString("def") should not(beIdenticalTo(jString("def")))
      }.message shouldBe Some(""""def" was no different to "def"""")
    }
  }

  "generic" - {
    "beDifferentTo" in {
      bob should beDifferentTo(sue)

      intercept[TestFailedException] {
        bob should beDifferentTo(bob)
      }.message shouldBe Some(s"""$bob was no different to $bob""")
    }

    "not beDifferentTo" in {
      bob should not(beDifferentTo(bob))

      intercept[TestFailedException] {
        bob should not(beDifferentTo(sue))
      }.message shouldBe Some(
        s"""$bob had the following differences with $sue:
          |  {
          |    "/age" : {
          |      "actual" : 11,
          |      "expected" : 22
          |    },
          |    "/name" : {
          |      "actual" : "bob",
          |      "expected" : "sue"
          |    },
          |    "/pet/age" : {
          |      "actual" : 1,
          |      "expected" : 2
          |    },
          |    "/pet/name" : {
          |      "actual" : "fido",
          |      "expected" : "fifi"
          |    }
          |  }""".stripMargin)
    }

    "beIdenticalTo" in {
      bob should beIdenticalTo(bob)

      intercept[TestFailedException] {
        bob should beIdenticalTo(sue)
      }.message shouldBe Some(
        s"""$bob had the following differences with $sue:
           |  {
           |    "/age" : {
           |      "actual" : 11,
           |      "expected" : 22
           |    },
           |    "/name" : {
           |      "actual" : "bob",
           |      "expected" : "sue"
           |    },
           |    "/pet/age" : {
           |      "actual" : 1,
           |      "expected" : 2
           |    },
           |    "/pet/name" : {
           |      "actual" : "fido",
           |      "expected" : "fifi"
           |    }
           |  }""".stripMargin)
    }

    "not beIdenticalTo" in {
      bob should not(beIdenticalTo(sue))

      intercept[TestFailedException] {
        bob should not(beIdenticalTo(bob))
      }.message shouldBe Some(s"""$bob was no different to $bob""")
    }
  }

  private val bob = Person(11, "bob", Dog(1, "fido"))
  private val sue = Person(22, "sue", Dog(2, "fifi"))
}