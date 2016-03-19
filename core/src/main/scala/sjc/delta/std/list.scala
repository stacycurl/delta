package sjc.delta.std

import sjc.delta.{Patch, Delta}
import sjc.delta.util.DeltaListOps


object list {
  implicit def deltaList[A, B](
    implicit deltaA: Delta.Aux[A, B], patchB: Patch[B]
  ): Delta.Aux[List[A], List[Change[A, B]]] = new Delta[List[A]] {
    type Out = List[Change[A, B]]

    def apply(left: List[A], right: List[A]): Out = {
      val (zipped, unzipped) = left.zipExact(right)

      val diffs: List[Change[A, B]] = zipped.zipWithIndex.flatMap {
        case ((lhs, rhs), index) ⇒ {
          val diff = deltaA(lhs, rhs)

          if (patchB.isEmpty(diff)) None else Some(Diff(index, diff))
        }
      }

      val zipLength = zipped.length

      unzipped match {
        case None ⇒ diffs
        case Some(Left(missing)) ⇒ diffs ++ missing.zipWithIndex.map { case (m, index) ⇒ Missing(index + zipLength, m) }
        case Some(Right(extra))  ⇒ diffs ++ extra.zipWithIndex.map   { case (e, index) ⇒ Extra(index + zipLength, e)   }
      }
    }
  }

  trait Change[+A, +B]
  case class Diff[B](index: Int, diff: B)           extends Change[Nothing, B]
  case class Extra[A](index: Int, extra: A)         extends Change[A, Nothing]
  case class Missing[A](index: Int, missing: A)     extends Change[A, Nothing]
}
