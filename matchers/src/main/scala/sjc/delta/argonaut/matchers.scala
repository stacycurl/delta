package sjc.delta.argonaut

import argonaut.{PrettyParams, EncodeJson, Json}
import org.scalatest.Matchers.not
import org.scalatest.matchers.{MatchResult, Matcher}
import sjc.delta.Delta
import sjc.delta.argonaut.json.actualExpected.flat.{jsonDelta, encodeJsonToDelta}


object matchers {
  def beIdenticalTo(expected: Json):             Matcher[Json] = not(beDifferentTo(expected))
  def beIdenticalTo[A: EncodeJson](expected: A): Matcher[A]    = not(beDifferentTo(expected))

  def beDifferentTo(expected: Json):             Matcher[Json] = DeltaMatcher(expected)
  def beDifferentTo[A: EncodeJson](expected: A): Matcher[A]    = DeltaMatcher(expected)

  private case class DeltaMatcher[A](expected: A)(implicit deltaA: Delta.Aux[A, Json]) extends Matcher[A] {
    def apply(actual: A): MatchResult = {
      val delta: Json = deltaA(actual, expected)

      MatchResult(
        delta != Json.jEmptyObject,
        s"$actual was no different to $expected",
        s"$actual had the following differences with $expected:\n  " + pretty(delta).replaceAllLiterally("\n", "\n  ")
      )
    }

    private def pretty(json: Json) = PrettyParams.spaces2.copy(preserveOrder = true).pretty(json)
  }
}