package sjc.delta.std

import sjc.delta.{Patch, Delta}


object set {
  implicit def deltaSet[A]: Delta.Aux[Set[A], SetPatch[A]] = new Delta[Set[A]] {
    type Out = SetPatch[A]

    def apply(left: Set[A], right: Set[A]): SetPatch[A] =
      SetPatch[A](added = right -- left, removed = left -- right)
  }

  case class SetPatch[A](added: Set[A], removed: Set[A]) {
    def isEmpty = added.isEmpty && removed.isEmpty
    def ignore(keys: Set[A]): SetPatch[A] = SetPatch(added -- keys, removed -- keys)
  }

  object SetPatch {
    implicit def setPatch[A]: Patch[SetPatch[A], A] = SetPatchInstance.asInstanceOf[Patch[SetPatch[A], A]]

    private val SetPatchInstance = Patch.create[SetPatch[Nothing], Nothing](
      _.isEmpty, _.toString, patch => paths => patch.ignore(paths)
    )
  }
}
