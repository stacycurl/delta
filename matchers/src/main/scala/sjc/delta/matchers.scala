package sjc.delta

import scala.language.implicitConversions

import org.scalatest.matchers.{MatchResult, Matcher}
import scala.reflect.ClassTag


object matchers {
  def beDifferentTo[A, B: Patch: Pretty](expected: A)(implicit deltaA: Delta.Aux[A, B]): DeltaMatcher[A, B] =
    new DeltaMatcher(expected, false)

  def beIdenticalTo[A, B: Patch: Pretty](expected: A)(implicit deltaA: Delta.Aux[A, B]): Matcher[A] =
    new DeltaMatcher(expected, true)


  class DeltaMatcher[A, B: Patch: Pretty](expected: A, positive: Boolean)(implicit deltaA: Delta.Aux[A, B])
    extends Matcher[A] {

    private[delta] def map[C: Patch: Pretty](f: B ⇒ C): DeltaMatcher[A, C] =
      new DeltaMatcher[A, C](expected, positive)(Patch[C], Pretty[C], deltaA.map(f))

    def withDelta[C: Patch: Pretty](expectedDelta: B)(implicit deltaC: Delta.Aux[B, C]): DeltaMatcher[A, C] =
      new DeltaMatcher[A, C](expected, true)(Patch[C], Pretty[B].to[C], deltaA.andThen(expectedDelta))

    def apply(actual: A): MatchResult = {
      val delta = deltaA(actual, expected)

      matchResult(delta, s"Detected the following differences:\n  ${Pretty[B].indent(delta)}")
    }

    private def matchResult(delta: B, positiveMsg: String) = {
      if (positive) MatchResult(Patch[B].isEmpty(delta),  positiveMsg, "No differences detected")
      else          MatchResult(Patch[B].nonEmpty(delta), "No differences detected", positiveMsg)
    }
  }

  trait Pretty[A] {
    def indent(a: A) = apply(a).replaceAllLiterally("\n", "\n  ")
    def apply(a: A): String
    def to[B: Pretty]: Pretty[B] = if (Pretty[B].classTag.equals(classTag)) this.asInstanceOf[Pretty[B]] else Pretty[B]

    protected val classTag: ClassTag[A]
  }

  object Pretty {
    def apply[A](implicit pretty: Pretty[A]): Pretty[A] = pretty

    def create[A: ClassTag](pretty: A ⇒ String): Pretty[A] = new Pretty[A] {
      def apply(a: A): String = pretty(a)
      protected val classTag: ClassTag[A] = implicitly[ClassTag[A]]
    }

    implicit def toStringPretty[A: ClassTag]: Pretty[A] = create[A](_.toString)
  }

  object syntax {
    import org.scalatest.Matchers._

    implicit def anyDeltaMatcherOps[A, B: Patch: Pretty](actual: A)(implicit delta: Delta.Aux[A, B])
      : AnyDeltaMatcherOps[A, B] = new AnyDeltaMatcherOps[A, B](actual)

    class AnyDeltaMatcherOps[A, B: Patch: Pretty](val actual: A)(implicit delta: Delta.Aux[A, B]) {
      def <=>(expected: A): Unit = actual should beIdenticalTo(expected)
      def </>(expected: A): Unit = actual should beDifferentTo(expected)
    }
  }
}