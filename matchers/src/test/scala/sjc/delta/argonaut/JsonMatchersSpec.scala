package sjc.delta.argonaut

import argonaut.{Json, JsonObject}
import argonaut.StringWrap.StringToStringWrap
import argonaut.Json.jString
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{FreeSpec, Matchers}
import sjc.delta.argonaut.matchers.{beConsistentWith, beDifferentTo, beIdenticalTo, prettyJson}
import sjc.delta.argonaut.json.actualExpected.flat.{encodeJsonToDelta, jsonDelta}
import sjc.delta.matchers.syntax.anyDeltaMatcherOps


class JsonMatchersSpec extends FreeSpec with Matchers with JsonSpecUtil {
  "json" - {
    "beDifferentTo" in {
      jString("def") should beDifferentTo(jString("abc"))

      intercept[TestFailedException] {
        jString("def") should beDifferentTo(jString("def"))
      }.message shouldBe Some("No differences detected")
    }

    "beDifferentTo.withDelta" in {
      jString("def") should beDifferentTo(jString("abc")).withDelta(parse("""
        |{
        |  "" : {
        |    "actual" : "def",
        |    "expected" : "abc"
        |  }
        |}""".stripMargin)
      )

      intercept[TestFailedException] {
        parse("""{"name": "bob"}""") should beDifferentTo(parse("""{"name": "sue"}""")).withDelta(parse("""
          |{
          |  "/name" : {
          |    "actual" : "bob",
          |    "expected" : "susan"
          |  }
          |}""".stripMargin)
        )
      }.message shouldBe Some(
        """Detected the following differences:
          |  {
          |    "/[/name]/expected" : {
          |      "actual" : "sue",
          |      "expected" : "susan"
          |    }
          |  }""".stripMargin
      )
    }

    "not beDifferentTo" in {
      jString("def") should not(beDifferentTo(jString("def")))

      intercept[TestFailedException] {
        jString("def") should not(beDifferentTo(jString("abc")))
      }.message shouldBe Some(
        """Detected the following differences:
          |  {
          |    "" : {
          |      "actual" : "def",
          |      "expected" : "abc"
          |    }
          |  }""".stripMargin
      )
    }

    "beIdenticalTo" in {
      jString("def") should beIdenticalTo(jString("def"))

      intercept[TestFailedException] {
        jString("def") should beIdenticalTo(jString("abc"))
      }.message shouldBe Some(
        """Detected the following differences:
          |  {
          |    "" : {
          |      "actual" : "def",
          |      "expected" : "abc"
          |    }
          |  }""".stripMargin
      )
    }

    "beIdenticalTo.ignoring" in {
      parse("""{"a": 1, "b": 2}""") should beIdenticalTo(parse("""{"a": 1, "b": 3}""")).ignoring("/b")

      intercept[TestFailedException] {
        parse("""{"a": 1, "b": 2, "c": 3}""") should beIdenticalTo(parse("""{"a": 1, "b": 3, "c": 4}""")).ignoring("/b")
      }.message shouldBe Some(
        """Detected the following differences:
          |  {
          |    "/c" : {
          |      "actual" : 3,
          |      "expected" : 4
          |    }
          |  }""".stripMargin
      )
    }

    "not beIdenticalTo" in {
      jString("def") should not(beIdenticalTo(jString("abc")))

      intercept[TestFailedException] {
        jString("def") should not(beIdenticalTo(jString("def")))
      }.message shouldBe Some("No differences detected")
    }

    "beConsistentWith" in {
      jString("def") should beConsistentWith(jString("def"))

      obj("a" := obj("ab" := 123)) should beConsistentWith(obj("a" := obj("ab" := 123), "b" := 123))

      intercept[TestFailedException] {
        jString("def") should beConsistentWith(jString("abc"))
      }.message shouldBe Some(
        """Detected the following inconsistencies:
          |  {
          |    "" : {
          |      "actual" : "def",
          |      "expected" : "abc"
          |    }
          |  }""".stripMargin
      )

      intercept[TestFailedException] {
        obj("a" := obj("ab" := 456)) should beConsistentWith(obj("a" := obj("ab" := 123), "b" := 123))
      }.message shouldBe Some(
        """Detected the following inconsistencies:
          |  {
          |    "/a/ab" : {
          |      "actual" : 456,
          |      "expected" : 123
          |    }
          |  }""".stripMargin
      )
    }
  }

  "syntax" - {
    "<=>" in {
      jString("def") <=> jString("def")

      intercept[TestFailedException] {
        jString("def") <=> jString("abc")
      }.message shouldBe Some(
        """Detected the following differences:
          |  {
          |    "" : {
          |      "actual" : "def",
          |      "expected" : "abc"
          |    }
          |  }""".stripMargin
      )
    }

    "</>" in {
      jString("def") </> jString("abc")

      intercept[TestFailedException] {
        jString("def") </> jString("def")
      }.message shouldBe Some("No differences detected")
    }
  }

  "generic" - {
    "beDifferentTo" in {
      bob should beDifferentTo(sue)

      intercept[TestFailedException] {
        bob should beDifferentTo(bob)
      }.message shouldBe Some("No differences detected")
    }

    "not beDifferentTo" in {
      bob should not(beDifferentTo(bob))

      intercept[TestFailedException] {
        bob should not(beDifferentTo(sue))
      }.message shouldBe Some(
        s"""Detected the following differences:
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
        s"""Detected the following differences:
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
      }.message shouldBe Some("No differences detected")
    }
  }

  private def obj(socks: Json.JsonAssoc*): Json = Json.jObject(JsonObject.fromTraversableOnce(socks))

  private val bob = Person(11, "bob", Dog(1, "fido"))
  private val sue = Person(22, "sue", Dog(2, "fifi"))
}