package zio.morphir.ir

trait InferredTypeOf[A] {
  def inferredType(value: A): UType
}

object InferredTypeOf {
  def apply[A](implicit ev: InferredTypeOf[A]): InferredTypeOf[A] = ev
}
