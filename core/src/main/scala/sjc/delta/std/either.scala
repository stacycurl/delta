package sjc.delta.std

import sjc.delta.{Patch, Delta}

object either {
  implicit def deltaEither[L, R, LOut, ROut](
    implicit ldelta: Delta.Aux[L, LOut], rdelta: Delta.Aux[R, ROut]
  ): Delta.Aux[Either[L, R], EitherPatch[L, R, LOut, ROut]] = {
    new Delta[Either[L, R]] {
      type Out = EitherPatch[L, R, LOut, ROut]

      def apply(eleft: Either[L, R], eright: Either[L, R]): EitherPatch[L, R, LOut, ROut] = (eleft, eright) match {
        case (Left(left),  Left(right))  ⇒ BothLeft[LOut](ldelta(left, right))
        case (Right(left), Right(right)) ⇒ BothRight[ROut](rdelta(left, right))
        case (Left(left),  Right(right)) ⇒ WasLeft(left, right)
        case (Right(left), Left(right))  ⇒ WasRight(left, right)
      }
    }
  }

  trait EitherPatch[+L, +R, +LOut, +ROut]
  case class BothLeft[LOut](out: LOut) extends EitherPatch[Nothing, Nothing, LOut, Nothing]
  case class BothRight[ROut](out: ROut) extends EitherPatch[Nothing, Nothing, Nothing, ROut]
  case class WasLeft[L, R](left: L, right: R) extends EitherPatch[L, R, Nothing, Nothing]
  case class WasRight[L, R](right: R, left: L) extends EitherPatch[L, R, Nothing, Nothing]

  object EitherPatch {
    implicit def eitherPatch[L, R, LOut: Patch, ROut: Patch]: Patch[EitherPatch[L, R, LOut, ROut]] =
      Patch.create[EitherPatch[L, R, LOut, ROut]](isEmptyFn = {
        case BothLeft(lout)  ⇒ Patch[LOut].isEmpty(lout)
        case BothRight(rout) ⇒ Patch[ROut].isEmpty(rout)
        case _               ⇒ false
      })
  }
}
