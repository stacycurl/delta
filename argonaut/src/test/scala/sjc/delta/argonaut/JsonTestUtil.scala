package sjc.delta.argonaut

import argonaut.{Json, Parse}
import org.junit.Assert._


object JsonTestUtil {
  def goCompare(actualJ: Json, expectedJ: Json): Unit = {
    val diffs = json.deltas(actualJ, expectedJ)

    if (diffs.nonEmpty) {
      fail(diffs.mkString("\n\n"))
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