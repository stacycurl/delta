package sjc.delta

import scala.reflect.ClassTag


package object std {
  object int {
    implicit val deltaInt: Delta.Aux[Int, Int] with Patch[Int, Unit] = new Delta[Int] with Patch[Int, Unit] {
      type Out = Int
      def apply(left: Int, right: Int): Int = right - left
      def isEmpty(i: Int): Boolean = i == 0
      def ignore(i: Int, paths: Unit*): Int = i
      def pretty(i: Int): String = i.toString

      protected val classTag: ClassTag[Int] = implicitly[ClassTag[Int]]
    }
  }
}