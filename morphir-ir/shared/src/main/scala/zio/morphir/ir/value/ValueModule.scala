package zio.morphir.ir.value

trait ValueModule {
  final type RawValue = zio.morphir.ir.value.RawValue

  def toRawValue[TA, VA](value: Value[TA, VA]): RawValue = ???
}
