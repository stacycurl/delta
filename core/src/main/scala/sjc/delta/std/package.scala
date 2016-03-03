package sjc.delta

package object std {
  object int {
    implicit val deltaInt: Delta.Aux[Int, Int] = new Delta[Int] {
      type Out = Int

      def apply(before: Int, after: Int): Int = after - before
    }
  }
}
