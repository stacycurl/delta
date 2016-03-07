package sjc.delta.std

import sjc.delta.Delta

object map {
  implicit def deltaMap[K, V, VOut](
    implicit deltaV: Delta.Aux[V, VOut]
  ): Delta.Aux[Map[K, V], MapPatch[K, V, VOut]] = new Delta[Map[K, V]] {
    type Out = MapPatch[K, V, VOut]

    def apply(left: Map[K, V], right: Map[K, V]): MapPatch[K, V, VOut] = {
      val changed: Map[K, VOut] = (left.keySet & right.keySet).map(k => {
        k -> deltaV(left(k), right(k))
      })(scala.collection.breakOut)

      MapPatch[K, V, VOut](right -- left.keySet, left -- right.keySet, changed)
    }
  }

  case class MapPatch[K, V, VOut](added: Map[K, V], removed: Map[K, V], changed: Map[K, VOut])
}
