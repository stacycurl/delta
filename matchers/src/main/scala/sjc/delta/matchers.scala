package sjc.delta

import org.scalatest.matchers.{MatchResult, Matcher}


object matchers {
  def beDifferentTo[A, B](expected: A)(implicit deltaA: DeltaWithZero.Aux[A, B], prettyB: Pretty[B]): DeltaMatcher[A, B] =
    new DeltaMatcher(expected, false)

  def beIdenticalTo[A, B](expected: A)(implicit deltaA: DeltaWithZero.Aux[A, B], prettyB: Pretty[B]): Matcher[A] =
    new DeltaMatcher(expected, true)


  class DeltaMatcher[A, B](
    expected: A, positive: Boolean
  )(implicit deltaA: DeltaWithZero.Aux[A, B], prettyB: Pretty[B]) extends Matcher[A] {

    def withDelta[C](expectedDelta: B)(implicit deltaC: DeltaWithZero.Aux[B, C], prettyC: Pretty[C]): DeltaMatcher[A, C] =
      new DeltaMatcher[A, C](expected, true)(deltaA.andThen(expectedDelta), prettyC)

    def apply(actual: A): MatchResult = {
      val delta = deltaA.apply(actual, expected)

      matchResult(delta, deltaA.zero, s"Detected the following differences:\n  ${prettyB.indent(delta)}")
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