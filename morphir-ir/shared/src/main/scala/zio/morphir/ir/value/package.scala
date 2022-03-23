package zio.morphir.ir

package object value {
  type RawValue = Value[Unit, Unit]
  val RawValue: Value.type = Value // TODO: Make a more tailored version

  type USpecification = Value[Unit, Unit]
  val USpecification: Specification.type = Specification

}
