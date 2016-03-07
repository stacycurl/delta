package sjc.delta.std

import sjc.delta.Delta


object set {
  implicit def deltaSet[A]: Delta.Aux[Set[A], SetPatch[A]] = new Delta[Set[A]] {
    type Out = SetPatch[A]

    def apply(left: Set[A], right: Set[A]): SetPatch[A] =
      SetPatch[A](added = right -- left, removed = left -- right)
  }

  case class SetPatch[A](added: Set[A], removed: Set[A])
}
