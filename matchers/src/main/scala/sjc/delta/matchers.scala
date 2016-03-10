package sjc.delta

import org.scalatest.matchers.{MatchResult, Matcher}


object matchers {
  def beDifferentTo[A, B](expected: A)(implicit deltaA: Delta.Aux[A, B]): DeltaMatcher[A, B] =
    new DeltaMatcher(expected, deltaA(expected, expected), None, (b: B) ⇒ b.toString, false)

  def beIdenticalTo[A, B](expected: A)(implicit deltaA: Delta.Aux[A, B]): Matcher[A] =
    new DeltaMatcher(expected, deltaA(expected, expected), None, (b: B) ⇒ b.toString, true)


  class DeltaMatcher[A, B](
    expected: A, empty: B, optExpectedDelta: Option[B], pretty: B ⇒ String, positive: Boolean
  )(implicit deltaA: Delta.Aux[A, B]) extends Matcher[A] {

    def withDelta(expectedDelta: B): DeltaMatcher[A, B] =
      new DeltaMatcher[A, B](expected, empty, Some(expectedDelta), pretty, true)

    def apply(actual: A): MatchResult = {
      val delta = deltaA(actual, expected)

      optExpectedDelta.fold(matchResult(delta, empty, s"Detected the following differences:\n  ${indent(delta)}"))(
        expectedDelta ⇒ matchResult(delta, expectedDelta,
          s"Difference was not as expected\n  actual: ${indent(delta)}\n  expected: ${indent(expectedDelta)}"
        )
      )
    }

    private def matchResult(delta: B, expectedDelta: B, positiveMsg: String) = MatchResult(
      if (positive) delta == expectedDelta    else delta != expectedDelta,
      if (positive) positiveMsg               else "No differences detected",
      if (positive) "No differences detected" else positiveMsg
    )

    private def indent(delta: B): String = pretty(delta).replaceAllLiterally("\n", "\n  ")
  }
}