package sjc.delta.std

import sjc.delta.{Patch, Delta}


object map {
  implicit def deltaMap[K, V, VOut](
    implicit deltaV: Delta.Aux[V, VOut]
  ): Delta.Aux[Map[K, V], MapPatch[K, V, VOut]] = new Delta[Map[K, V]] {
    type Out = MapPatch[K, V, VOut]

    def apply(left: Map[K, V], right: Map[K, V]): MapPatch[K, V, VOut] = {
      val changed: Map[K, VOut] = (left.keySet & right.keySet).map(k => {
        k -> deltaV(left(k), right(k))
      }).toMap

      MapPatch[K, V, VOut](right -- left.keySet, left -- right.keySet, changed)
    }
  }

  case class MapPatch[K, V, VOut](added: Map[K, V], removed: Map[K, V], changed: Map[K, VOut]) {
    def isEmpty = added.isEmpty && removed.isEmpty && changed.isEmpty
    def ignore(keys: Set[K]): MapPatch[K, V, VOut] = MapPatch(added -- keys, removed -- keys, changed -- keys)
  }

  object MapPatch {
    implicit def mapPatch[K, V, VOut]: Patch[MapPatch[K, V, VOut], K] =
      MapPatchInstance.asInstanceOf[Patch[MapPatch[K, V, VOut], K]]

    private val MapPatchInstance = Patch.create[MapPatch[Nothing, Nothing, Nothing], Nothing](
      _.isEmpty, _.toString, patch => paths => patch.ignore(paths)
    )
  }
}