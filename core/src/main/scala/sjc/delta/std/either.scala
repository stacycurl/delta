package sjc.delta.std

import sjc.delta.Delta

object either {
  implicit def deltaEither[L, R, LOut, ROut](
    implicit ldelta: Delta.Aux[L, LOut], rdelta: Delta.Aux[R, ROut]
  ): Delta.Aux[Either[L, R], EitherPatch[L, R, LOut, ROut]] = {
    new Delta[Either[L, R]] {
      type Out = EitherPatch[L, R, LOut, ROut]

      def apply(ebefore: Either[L, R], eafter: Either[L, R]): EitherPatch[L, R, LOut, ROut] = {
        (ebefore, eafter) match {
          case (Left(before),  Left(after))  => BothLeft[LOut](ldelta(before, after))
          case (Right(before), Right(after)) => BothRight[ROut](rdelta(before, after))
          case (Left(before),  Right(after)) => WasLeft(before, after)
          case (Right(before), Left(after))  => WasRight(before, after)
        }
      }
    }
  }

  trait EitherPatch[+L, +R, +LOut, +ROut]
  case class BothLeft[LOut](out: LOut) extends EitherPatch[Nothing, Nothing, LOut, Nothing]
  case class BothRight[ROut](out: ROut) extends EitherPatch[Nothing, Nothing, Nothing, ROut]
  case class WasLeft[L, R](left: L, right: R) extends EitherPatch[L, R, Nothing, Nothing]
  case class WasRight[L, R](right: R, left: L) extends EitherPatch[L, R, Nothing, Nothing]
}
