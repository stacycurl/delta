package sjc.delta

import scala.language.implicitConversions

import org.scalatest.matchers.{MatchResult, Matcher}


object matchers {
  def beDifferentTo[A, B, PathB](expected: A)(
    implicit deltaA: Delta.Aux[A, B], patchB: Patch[B, PathB]
  ): DeltaMatcher[A, B, PathB] = new DeltaMatcher[A, B, PathB](expected, false)

  def beIdenticalTo[A, B, PathB](expected: A)(
    implicit deltaA: Delta.Aux[A, B], patchB: Patch[B, PathB]
  ): DeltaMatcher[A, B, PathB] = new DeltaMatcher[A, B, PathB](expected, true)


  class DeltaMatcher[A, B, PathB](expected: A, positive: Boolean)(
    implicit deltaA: Delta.Aux[A, B], patchB: Patch[B, PathB]
  ) extends Matcher[A] {

    def withDelta[C, PathC](expectedDelta: B)(
      implicit deltaC: Delta.Aux[B, C], patchC: Patch[C, PathC]
    ): DeltaMatcher[A, C, PathC] =
      new DeltaMatcher[A, C, PathC](expected, true)(deltaA.andThen(expectedDelta), patchB.to[C, PathC])

    def ignoring(paths: PathB*): DeltaMatcher[A, B, PathB] = map(b ⇒ patchB.ignore(b, paths: _*))

    def apply(actual: A): MatchResult = {
      val delta = deltaA(actual, expected)

      matchResult(delta, s"Detected the following differences:\n  ${patchB.indent(delta)}")
    }

    private def matchResult(delta: B, positiveMsg: String) = {
      if (positive) MatchResult(patchB.isEmpty(delta),  positiveMsg, "No differences detected")
      else          MatchResult(patchB.nonEmpty(delta), "No differences detected", positiveMsg)
    }

    private[delta] def map[C, PathC](f: B ⇒ C)(implicit patchC: Patch[C, PathC]): DeltaMatcher[A, C, PathC] =
      new DeltaMatcher[A, C, PathC](expected, positive)(deltaA.map(f), patchC)
  }

  object syntax {
    import org.scalatest.Matchers._

    implicit def anyDeltaMatcherOps[A, B, PathB](actual: A)(
      implicit delta: Delta.Aux[A, B], patchB: Patch[B, PathB]
    ): AnyDeltaMatcherOps[A, B, PathB] = new AnyDeltaMatcherOps[A, B, PathB](actual)

    class AnyDeltaMatcherOps[A, B, PathB](val actual: A)(implicit delta: Delta.Aux[A, B], patchB: Patch[B, PathB]) {
      def <=>(expected: A): Unit = actual should beIdenticalTo(expected)
      def </>(expected: A): Unit = actual should beDifferentTo(expected)
    }
  }
}