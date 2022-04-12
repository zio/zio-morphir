package zio.morphir.ir.value.recursive

trait ValueModule extends ValueConstructors {
  final def uncurryApply[TA, VA](
      fun: Value[TA, VA],
      lastArg: Value[TA, VA]
  ): (Value[TA, VA], scala.List[Value[TA, VA]]) =
    fun.uncurryApply(lastArg)
}
