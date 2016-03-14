package sjc.delta.scalaz

import sjc.delta.Delta
import sjc.delta.Delta.DeltaDeltaOps

import scalaz.\/

object disjunction {
  implicit def deltaV[L, R](implicit deltaEither: Delta[Either[L, R]]): Delta[L \/ R] =
    deltaEither.contramap[L \/ R](_.toEither)
}
