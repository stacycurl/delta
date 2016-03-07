package sjc.delta

package object std {
  object int {
    implicit val deltaInt: Delta.Aux[Int, Int] = new Delta[Int] {
      type Out = Int

      def apply(left: Int, right: Int): Int = right - left
    }
  }
}
