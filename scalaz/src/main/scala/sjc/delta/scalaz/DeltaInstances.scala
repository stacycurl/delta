package sjc.delta.scalaz

import sjc.delta.Delta
import sjc.delta.Delta._
import sjc.delta.std.either._

import scalaz._


object DeltaInstances {
  implicit def deltaFunctor[In]: Functor[Delta.Aux[In, ?]] = new Functor[Delta.Aux[In, ?]] {
    def map[A, B](delta: Aux[In, A])(f: A ⇒ B): Aux[In, B] = delta.map(f)
  }

  implicit def deltaContravariant[Out]: Contravariant[Delta.Aux[?, Out]] = new Contravariant[Delta.Aux[?, Out]] {
    def contramap[A, B](delta: Aux[A, Out])(f: B ⇒ A): Aux[B, Out] = delta.contramap(f)
  }

  implicit val deltaProfunctor: Profunctor[Delta.Aux] = new Profunctor[Aux] {
    def mapfst[A, B, C](delta: Aux[A, B])(f: C ⇒ A): Aux[C, B] = delta.contramap(f)
    def mapsnd[A, B, C](delta: Aux[A, B])(f: B ⇒ C): Aux[A, C] = delta.map(f)
    override def dimap[A, B, C, D](delta: Aux[A, B])(f: C ⇒ A)(g: B ⇒ D): Aux[C, D] = delta.dimap(f, g)
  }

  implicit def deltaZip[In]: Zip[Delta.Aux[In, ?]] = new Zip[Delta.Aux[In, ?]] {
    def zip[A, B](deltaToA: ⇒ Aux[In, A], deltaToB: ⇒ Aux[In, B]): Aux[In, (A, B)] =
      Delta.from[In].curried(left ⇒ right ⇒ (deltaToA(left, right), deltaToB(left, right)))
  }

  implicit def deltaRepresentable[In]: Representable[Delta.Aux[In, ?], (In, In)] =
    new Representable[Delta.Aux[In, ?], (In, In)] {
      def rep[Out](f: ((In, In)) ⇒ Out): Aux[In, Out] =
        Delta.from[In].curried(left ⇒ right ⇒ f((left, right)))

      def unrep[Out](f: Aux[In, Out]): ((In, In)) ⇒ Out = (f.apply _).tupled
    }

  object leftCoZip {
    implicit def deltaLeftCozip[Out]: Cozip[Delta.Aux[?, Out]] = new Cozip[Delta.Aux[?, Out]] {
      def cozip[A, B](deltaAOrB: Aux[A \/ B, Out]): Aux[A, Out] \/ Aux[B, Out] =
        -\/(Delta.from[A].curried(left ⇒ right ⇒ deltaAOrB(-\/(left), -\/(right))))
    }
  }

  object rightCozip {
    implicit def deltaLeftCozip[Out]: Cozip[Delta.Aux[?, Out]] = new Cozip[Delta.Aux[?, Out]] {
      def cozip[A, B](deltaAOrB: Aux[A \/ B, Out]): Aux[A, Out] \/ Aux[B, Out] =
        \/-(Delta.from[B].curried(left ⇒ right ⇒ deltaAOrB(\/-(left), \/-(right))))
    }
  }


  implicit def eitherPatchEqual[L, R, LOut, ROut](
    implicit lEqual: Equal[L], rEqual: Equal[R], loutEqual: Equal[LOut], routEqual: Equal[ROut]
  ): Equal[EitherPatch[L, R, LOut, ROut]] = new Equal[EitherPatch[L, R, LOut, ROut]] {
    type EP = EitherPatch[L, R, LOut, ROut]

    def equal(left: EP, right: EP): Boolean = (left, right) match {
      case (BothLeft(blLeft), BothLeft(blRight)) ⇒ {
        loutEqual.equal(blLeft, blRight)
      }
      case (BothRight(brLeft), BothRight(brRight)) ⇒ {
        routEqual.equal(brLeft, brRight)
      }
      case (WasLeft(lLeft, rLeft), WasLeft(lRight, rRight)) ⇒ {
        lEqual.equal(lLeft, lRight) && rEqual.equal(rLeft, rRight)
      }
      case (WasRight(rLeft, lLeft), WasRight(rRight, lRight)) ⇒ {
        rEqual.equal(rLeft, rRight) && lEqual.equal(lLeft, lRight)
      }
      case _ ⇒ false
    }
  }

  implicit def eitherPatchShow[L, R, LOut, ROut](
    implicit lshow: Show[L], rshow: Show[R], loutShow: Show[LOut], routShow: Show[ROut]
  ): Show[EitherPatch[L, R, LOut, ROut]] = new Show[EitherPatch[L, R, LOut, ROut]] {
    override def shows(in: EitherPatch[L, R, LOut, ROut]): String = in match {
      case BothLeft(out)         ⇒ s"BothLeft(${loutShow.show(out)})"
      case BothRight(out)        ⇒ s"BothRight(${routShow.show(out)})"
      case WasLeft(left, right)  ⇒ s"WasLeft(${lshow.show(left)}, ${rshow.show(right)})"
      case WasRight(right, left) ⇒ s"WasRight(${rshow.show(right)}, S{lshow.show(left)})"
    }
  }

}
