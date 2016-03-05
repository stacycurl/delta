package sjc.delta.argonaut

import argonaut.{Parse, CodecJson, Json}
import argonaut.Json.{jArray, jBool, jEmptyObject, jNull, jNumber, jString}
import org.junit.Test
import sjc.delta.TestUtil
import sjc.delta.Delta.DeltaOps
import sjc.delta.argonaut.json.flat.jsonDelta


class JsonTest extends TestUtil {
  @Test def shouldIgnoreIdenticalElements(): Unit = {
    jNull          delta jNull          shouldEqual jEmptyObject
    jBool(true)    delta jBool(true)    shouldEqual jEmptyObject
    jNumber(123)   delta jNumber(123)   shouldEqual jEmptyObject
    jString("abc") delta jString("abc") shouldEqual jEmptyObject
    jEmptyObject   delta jEmptyObject   shouldEqual jEmptyObject
    jArray(Nil)    delta jArray(Nil)    shouldEqual jEmptyObject

    parse("""["abc"]""")                      delta parse("""["abc"]""")                      shouldEqual jEmptyObject
    parse("""{"foo": "abc"}""")               delta parse("""{"foo": "abc"}""")               shouldEqual jEmptyObject
    parse("""{"foo": "abc", "bar": "abc"}""") delta parse("""{"foo": "abc", "bar": "abc"}""") shouldEqual jEmptyObject
  }

  @Test def shouldListDifferentElements(): Unit =  {
    jString("abc")       delta jString("def")       shouldEqual parse("""{"": {"expected": "def", "actual": "abc"}}""")
    parse("""["abc"]""") delta parse("""["def"]""") shouldEqual parse("""{"0": {"expected": "def", "actual": "abc"}}""")

    parse("""{"foo": "abc"}""") delta parse("""{"foo": "def"}""") shouldEqual parse(
      """{"foo": {"expected": "def", "actual": "abc"}}"""
    )
  }

  @Test def shouldListMissingElements(): Unit =  {
    parse("{}") delta parse("""{"parent": "def"}""") shouldEqual parse("""{"parent": {"expected": "def"}}""")
    parse("[]") delta parse("""["def"]""")           shouldEqual parse("""{"0": {"expected": "def"}}""")
  }

  @Test def shouldListExtraElements(): Unit = {
    parse("""{"parent": "def"}""") delta parse("{}") shouldEqual parse("""{"parent": {"actual": "def"}}""")
    parse("""["def"]""")           delta parse("[]") shouldEqual parse("""{"0": {"actual": "def"}}""")
  }

  @Test def genericDelta(): Unit = {
    import sjc.delta.argonaut.json.generic.flat.encodeJsonToDelta

    case class Person(age: Int, name: String)
    implicit val codecFoo: CodecJson[Person] = CodecJson.casecodec2(Person.apply, Person.unapply)("age", "name")

    Person(1, "foo") delta Person(2, "bar") shouldEqual parse(
      """{"age": {"expected": 2, "actual": 1}, "name": {"expected": "bar", "actual": "foo"}}"""
    )
  }

  private def parse(content: String): Json = Parse.parseOption(content).getOrElse(sys.error("not json"))
}
