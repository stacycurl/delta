package sjc.delta.scalaz

import scala.language.higherKinds

import sjc.delta.Delta
import sjc.delta.Delta.Aux
import sjc.delta.std.either.{BothLeft, BothRight, EitherPatch, WasLeft, WasRight}
import scalaz.{-\/, Applicative, Contravariant, Cord, Cozip, Equal, Monad, Profunctor, Representable, Show, Unzip, Zip, \/, \/-}


object instances {
  implicit def deltaApplicative[In]: Applicative[Aux[In, ?]] = new Applicative[Aux[In, ?]] {
    def ap[A, B](deltaA: ⇒ Aux[In, A])(deltaAB: ⇒ Aux[In, A ⇒ B]): Aux[In, B] = deltaA.applyTo(deltaAB)
    override def map[A, B](delta: Aux[In, A])(f: A ⇒ B): Aux[In, B] = delta.map(f)
    def point[A](a: ⇒ A): Aux[In, A] = Delta.const[In, A](a)
  }

  implicit def deltaMonad[In]: Monad[Aux[In, ?]] = new Monad[Aux[In, ?]] {
    def point[A](a: ⇒ A): Aux[In, A] = Delta.const[In, A](a)
    def bind[A, B](deltaA: Aux[In, A])(aDeltaB: A ⇒ Aux[In, B]): Aux[In, B] = deltaA.flatMap(aDeltaB)
  }

  implicit def deltaContravariant[Out]: Contravariant[Aux[?, Out]] = new Contravariant[Aux[?, Out]] {
    def contramap[A, B](delta: Aux[A, Out])(f: B ⇒ A): Aux[B, Out] = delta.contramap(f)
  }

  implicit val deltaProfunctor: Profunctor[Aux] = new Profunctor[Aux] {
    def mapfst[A, B, C](delta: Aux[A, B])(f: C ⇒ A): Aux[C, B] = delta.contramap(f)
    def mapsnd[A, B, C](delta: Aux[A, B])(f: B ⇒ C): Aux[A, C] = delta.map(f)
    override def dimap[A, B, C, D](delta: Aux[A, B])(f: C ⇒ A)(g: B ⇒ D): Aux[C, D] = delta.dimap(f, g)
  }

  implicit def deltaZip[In]: Zip[Aux[In, ?]] = new Zip[Aux[In, ?]] {
    def zip[A, B](deltaToA: ⇒ Aux[In, A], deltaToB: ⇒ Aux[In, B]): Aux[In, (A, B)] = deltaToA.zip(deltaToB)
  }

  implicit def deltaUnzip[In]: Unzip[Aux[In, ?]] = new Unzip[Aux[In, ?]] {
    def unzip[A, B](deltaAB: Aux[In, (A, B)]): (Aux[In, A], Aux[In, B]) = deltaAB.unzip
  }

  implicit def deltaRepresentable[In]: Representable[Aux[In, ?], (In, In)] =
    new Representable[Aux[In, ?], (In, In)] {
      def rep[Out](f: ((In, In)) ⇒ Out): Aux[In, Out] =
        Delta.from[In].curried(left ⇒ right ⇒ f((left, right)))

      def unrep[Out](f: Aux[In, Out]): ((In, In)) ⇒ Out = (f.apply _).tupled
    }

  object leftCoZip {
    implicit def deltaLeftCozip[Out]: Cozip[Aux[?, Out]] = new Cozip[Aux[?, Out]] {
      def cozip[A, B](deltaAOrB: Aux[A \/ B, Out]): Aux[A, Out] \/ Aux[B, Out] =
        -\/(Delta.from[A].curried(left ⇒ right ⇒ deltaAOrB(-\/(left), -\/(right))))
    }
  }

  object rightCozip {
    implicit def deltaLeftCozip[Out]: Cozip[Aux[?, Out]] = new Cozip[Aux[?, Out]] {
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
  ): Show[EitherPatch[L, R, LOut, ROut]] = (in: EitherPatch[L, R, LOut, ROut]) => Cord(in match {
    case BothLeft(out) ⇒ s"BothLeft(${loutShow.show(out)})"
    case BothRight(out) ⇒ s"BothRight(${routShow.show(out)})"
    case WasLeft(left, right) ⇒ s"WasLeft(${lshow.show(left)}, ${rshow.show(right)})"
    case WasRight(right, left) ⇒ s"WasRight(${rshow.show(right)}, ${lshow.show(left)})"
  })
}