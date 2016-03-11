package sjc.delta.std

import sjc.delta.DeltaWithZero


object set {
  implicit def deltaSet[A]: DeltaWithZero.Aux[Set[A], SetPatch[A]] = new DeltaWithZero[Set[A]] {
    type Out = SetPatch[A]

    val zero: SetPatch[A] = SetPatch[A](Set(), Set())

    def apply(left: Set[A], right: Set[A]): SetPatch[A] =
      SetPatch[A](added = right -- left, removed = left -- right)
  }

  case class SetPatch[A](added: Set[A], removed: Set[A])
}
