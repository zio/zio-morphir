package zio.morphir.ir

package object value {

  type RawValue = Value[Unit, Any]
  val RawValue: Value.type = Value

  type TypedValue = Value[Unit, UType]
  val TypedValue: Value.type = Value

  type UDefinition = Definition[Unit, Unit]
  val UDefinition: Definition.type = Definition

  type UPattern = Pattern[Any]
  val UPattern: Pattern.type = Pattern

  type USpecification = Value[Unit, Unit]
  val USpecification: Specification.type = Specification

}
