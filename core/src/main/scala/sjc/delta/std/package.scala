package sjc.delta

package object std {
  object int {
    implicit val deltaInt: Delta.Aux[Int, Int] with Patch[Int] = new Delta[Int] with Patch[Int] {
      type Out = Int

      def apply(left: Int, right: Int): Int = right - left
      def isEmpty(i: Int): Boolean = i == 0
    }
  }
}
