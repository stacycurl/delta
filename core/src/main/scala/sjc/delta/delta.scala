package sjc.delta

import scala.language.implicitConversions
import scala.reflect.ClassTag


trait Delta[In] {
  type Out

  def tupled(lr: (In, In)): Out = apply(lr._1, lr._2)
  def apply(left: In, right: In): Out
}

object Delta {
  def apply[In](implicit delta: Delta[In]): Aux[In, delta.Out] = delta

  type Aux[In, Out0] = Delta[In] { type Out = Out0 }

  def from[In] = new From[In]
  def const[In, A](a: A): Aux[In, A] = new Delta.Const[In, A](a)

  class From[In] {
    def curried[Out](f: In ⇒ In ⇒ Out): Aux[In, Out] = apply(Function.uncurried(f))
    def apply[Out](f: (In, In) ⇒ Out): Aux[In, Out] = new Function[In, Out](f)
  }

  implicit class DeltaDeltaOps[A, B](val delta: Aux[A, B]) extends AnyVal {
    def map[C](f: B ⇒ C):                      Aux[A, C]             = new Delta.Mapped[A, B, C](delta, f)
    def flatMap[C](f: B ⇒ Aux[A, C]):          Aux[A, C]             = new Delta.FlatMapped[A, B, C](delta, f)
    def contramap[In](f: In ⇒ A):              Aux[In, B]            = new Delta.Contramapped[A, B, In](delta, f)
    def dimap[In, C](f: In ⇒ A, g: B ⇒ C):     Aux[In, C]            = new Delta.DiMapped[In, A, B, C](delta, f, g)
    def zip[C](other: Aux[A, C]):              Aux[A, (B, C)]        = new Delta.Zipped[A, B, C](delta, other)
    def applyTo[C](other: Aux[A, B ⇒ C]):      Aux[A, C]             = new Delta.Apply[A, B, C](other, delta)
    def andThen[C](out: B)(implicit deltaOut2: Aux[B, C]): Aux[A, C] = new Delta.AndThen[A, B, C](out, delta, deltaOut2)
    def lift[In]:                              Aux[In ⇒ A, In ⇒ B]   = new Delta.Lift[In, A, B](delta)
    def ***[C, D](other: Aux[C, D]):           Aux[(A, C), (B, D)]   = new Delta.Split[A, B, C, D](delta, other)
  }

  implicit class DeltaToTupleDeltaOps[A, B, C](val delta: Aux[A, (B, C)]) extends AnyVal {
    def unzip: (Aux[A, B], Aux[A, C]) = (_1, _2)
    def _1: Aux[A, B] = Delta.from[A].curried(left ⇒ right ⇒ delta(left, right)._1)
    def _2: Aux[A, C] = Delta.from[A].curried(left ⇒ right ⇒ delta(left, right)._2)
  }

  implicit class DeltaOps[In](val left: In) extends AnyVal {
    def delta(right: In)(implicit delta: Delta[In]): delta.Out = delta(left, right)
  }

  object function {
    implicit def lift[A, B, C](implicit delta: Aux[B, C]): Aux[A ⇒ B, A ⇒ C] = delta.lift[A]
  }

  object fallback {
    implicit def fallbackDelta[A]: Aux[A, (A, A)] with Patch[(A, A), Unit] = new Delta[A] with Patch[(A, A), Unit] {
      type Out = (A, A)
      def apply(left: A, right: A): (A, A) = (left, right)
      def isEmpty(patch: (A, A)): Boolean = patch._1 == patch._2
      def pretty(p: (A, A)): String = p.toString
      def ignore(p: (A, A), paths: Unit*): (A, A) = p

      protected val classTag: ClassTag[(A, A)] = implicitly[ClassTag[(A, A)]]
    }
  }

  private class Function[In, Out0](f: (In, In) ⇒ Out0) extends Delta[In] {
    type Out = Out0
    def apply(left: In, right: In): Out = f(left, right)
  }

  private class Const[A, B](b: B) extends Delta[A] {
    type Out = B
    def apply(left: A, right: A): Out = b
  }

  private class Mapped[A, B, C](delta: Aux[A, B], f: B ⇒ C) extends Delta[A] {
    type Out = C
    def apply(left: A, right: A): Out = f(delta(left, right))
  }

  private class FlatMapped[A, B, C](delta: Aux[A, B], f: B ⇒ Aux[A, C]) extends Delta[A] {
    type Out = C
    def apply(left: A, right: A): C = f(delta(left, right))(left, right)
  }

  private class Contramapped[A, B, C](delta: Aux[A, B], f: C ⇒ A) extends Delta[C] {
    type Out = B
    def apply(left: C, right: C): Out = delta(f(left), f(right))
  }

  private class DiMapped[A, B, C, D](delta: Aux[B, C], f: A ⇒ B, g: C ⇒ D) extends Delta[A] {
    type Out = D
    def apply(left: A, right: A): D = g(delta(f(left), f(right)))
  }

  private class Split[A, B, C, D](deltaAB: Aux[A, B], deltaCD: Aux[C, D]) extends Delta[(A, C)] {
    type Out = (B, D)
    def apply(left: (A, C), right: (A, C)): Out = (deltaAB(left._1, right._1), deltaCD(left._2, right._2))
  }

  private class Zipped[A, B, C](deltaAB: Aux[A, B], deltaCD: Aux[A, C]) extends Delta[A] {
    type Out = (B, C)
    def apply(left: A, right: A): Out = (deltaAB(left, right), deltaCD(left, right))
  }

  private class Apply[A, B, C](deltaABC: Aux[A, B ⇒ C], deltaAB: Aux[A, B]) extends Delta[A] {
    type Out = C
    def apply(left: A, right: A): C = deltaABC(left, right).apply(deltaAB(left, right))
  }

  private class AndThen[In, Out1, Out2](
    out1: Out1, first: Aux[In, Out1], second: Aux[Out1, Out2]
  ) extends Delta[In] {

    type Out = Out2

    def apply(left: In, right: In): Out2 = {
      val firstOrder  = first.apply(left, right)
      val secondOrder = second.apply(firstOrder, out1)

      secondOrder
    }
  }

  private class Lift[A, B, C](delta: Aux[B, C]) extends Delta[A ⇒ B] {
    type Out = A ⇒ C
    def apply(left: A ⇒ B, right: A ⇒ B): Out = (a: A) ⇒ delta(left(a), right(a))
  }
}

trait Patch[P, Path] {
  def nonEmpty(patch: P): Boolean = !isEmpty(patch)
  def isEmpty(patch: P): Boolean

  def indent(p: P) = pretty(p).replaceAllLiterally("\n", "\n  ")
  def pretty(p: P): String

  def ignore(p: P, paths: Path*): P

  def to[B, PathB](implicit patchB: Patch[B, PathB]): Patch[B, PathB] =
    if (Patch[B, PathB].classTag.equals(classTag)) this.asInstanceOf[Patch[B, PathB]] else Patch[B, PathB]

  protected val classTag: ClassTag[P]
}

object Patch {
  def isEmpty[P, Path](p: P)(implicit patch: Patch[P, Path]) = patch.isEmpty(p)

  def fromExample[P, Path](p: P)(implicit patch: Patch[P, Path]): Patch[P, Path] = patch

  def apply[P, Path](implicit patch: Patch[P, Path]): Patch[P, Path] = patch

  def create[P: ClassTag, PathP](
    isEmptyFn: P ⇒ Boolean, prettyFn: P ⇒ String, ignoreFn: P ⇒ Set[PathP] ⇒ P
  ): Patch[P, PathP] = new Patch[P, PathP] {
    def isEmpty(patch: P): Boolean = isEmptyFn(patch)
    def pretty(p: P): String = prettyFn(p)
    def ignore(p: P, paths: PathP*): P = ignoreFn(p)(paths.toSet)

    protected val classTag: ClassTag[P] = implicitly[ClassTag[P]]
  }
}