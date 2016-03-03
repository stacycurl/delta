package sjc.delta.std

import sjc.delta.Delta

object map {
  implicit def deltaMap[K, V, VOut](
    implicit deltaV: Delta.Aux[V, VOut]
  ): Delta.Aux[Map[K, V], MapPatch[K, V, VOut]] = new Delta[Map[K, V]] {
    type Out = MapPatch[K, V, VOut]

    def apply(before: Map[K, V], after: Map[K, V]): MapPatch[K, V, VOut] = {
      val changed: Map[K, VOut] = (before.keySet & after.keySet).map(k => {
        k -> deltaV(before(k), after(k))
      })(scala.collection.breakOut)

      MapPatch[K, V, VOut](after -- before.keySet, before -- after.keySet, changed)
    }
  }

  case class MapPatch[K, V, VOut](added: Map[K, V], removed: Map[K, V], changed: Map[K, VOut])
}
