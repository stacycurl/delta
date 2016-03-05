package sjc.delta.argonaut

import argonaut.{Json, Parse}
import org.junit.Assert._
import sjc.delta.TestUtil


trait JsonTestUtil extends TestUtil {
  implicit class JsonTestOps(json: Json) {
    def jsonShouldEqual(expected: Json) = json.spaces2 shouldEqual expected.spaces2
  }

  def goCompare(actualJ: Json, expectedJ: Json): Unit = {
    val diffs = json.compressed.jsonDelta.apply(actualJ, expectedJ)

    if (diffs != Json.array()) {
      fail(diffs.spaces2)
    }
  }

  def goCompare(actual: String, expected: String): Unit = {
    lazy val dumpBoth: String = "\nActual is \"" + actual + "\"\nExpected is \"" + expected + "\""

    for {
      actualJ   ← Parse.parse(actual).leftMap(error ⇒ fail(s"""Failed to parse actual: "$error" $dumpBoth"""))
      expectedJ ← Parse.parse(expected).leftMap(error ⇒ fail(s"""Failed to parse expected: "$error" $dumpBoth"""))
    } yield goCompare(actualJ, expectedJ)
  }
}