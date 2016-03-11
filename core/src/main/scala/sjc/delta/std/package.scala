package sjc.delta

package object std {
  object int {
    implicit val deltaInt: DeltaWithZero.Aux[Int, Int] = new DeltaWithZero[Int] {
      type Out = Int

      def apply(left: Int, right: Int): Int = right - left
      val zero: Int = 0
    }
  }
}
