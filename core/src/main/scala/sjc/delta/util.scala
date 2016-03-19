package sjc.delta

import scala.annotation.tailrec


private[delta] object util {
  implicit class DeltaListOps[A](list: List[A]) {
    import scala.collection.immutable.{::, List, Nil}

    def zipExact[B](other: List[B]): (List[(A, B)], Option[Either[List[A], List[B]]]) = {
      @tailrec
      def recurse(as: List[A], bs: List[B], abs: List[(A,B)]): (List[(A,B)], Option[Either[List[A], List[B]]]) = {
        (as, bs) match {
          case (l :: left, r :: right) ⇒ recurse(left, right, (l, r) :: abs)
          case (Nil, Nil)              ⇒ (abs.reverse, None)
          case (left, Nil)             ⇒ (abs.reverse, Some(Left(left)))
          case (Nil, right)            ⇒ (abs.reverse, Some(Right(right)))
        }
      }

      recurse(list, other, Nil)
    }
  }
}