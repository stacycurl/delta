package sjc.delta

import org.scalatest.matchers.{MatchResult, Matcher}


object matchers {
  def beDifferentTo[A, B](expected: A)(implicit deltaA: DeltaWithZero.Aux[A, B], prettyB: Pretty[B]): DeltaMatcher[A, B] =
    new DeltaMatcher(expected, None, false)

  def beIdenticalTo[A, B](expected: A)(implicit deltaA: DeltaWithZero.Aux[A, B], prettyB: Pretty[B]): Matcher[A] =
    new DeltaMatcher(expected, None, true)


  class DeltaMatcher[A, B](
    expected: A, optExpectedDelta: Option[B], positive: Boolean
  )(implicit deltaA: DeltaWithZero.Aux[A, B], prettyB: Pretty[B]) extends Matcher[A] {

    def withDelta(expectedDelta: B): DeltaMatcher[A, B] =
      new DeltaMatcher[A, B](expected, Some(expectedDelta), true)

    def apply(actual: A): MatchResult = {
      val delta = deltaA(actual, expected)

      optExpectedDelta.fold(matchResult(delta, deltaA.zero, s"Detected the following differences:\n  ${prettyB.indent(delta)}"))(
        expectedDelta ⇒ matchResult(delta, expectedDelta,
          s"Difference was not as expected\n  actual: ${prettyB.indent(delta)}\n  expected: ${prettyB.indent(expectedDelta)}"
        )
      )
    }

    private def matchResult(delta: B, expectedDelta: B, positiveMsg: String) = MatchResult(
      if (positive) delta == expectedDelta    else delta != expectedDelta,
      if (positive) positiveMsg               else "No differences detected",
      if (positive) "No differences detected" else positiveMsg
    )
  }

  trait Pretty[A] {
    def indent(a: A) = apply(a).replaceAllLiterally("\n", "\n  ")
    def apply(a: A): String
  }

  object Pretty {
    def apply[A](implicit pretty: Pretty[A]): Pretty[A] = pretty

    def create[A](pretty: A ⇒ String): Pretty[A] = new Pretty[A] {
      def apply(a: A): String = pretty(a)
    }

    implicit def toStringPretty[A]: Pretty[A] = create[A](_.toString)
  }
}