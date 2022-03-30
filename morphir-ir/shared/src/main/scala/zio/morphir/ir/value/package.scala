package zio.morphir.ir
import zio.prelude.AnyType
package object value {

  type RawValue = Value[AnyType, Any, Any]
  val RawValue: Value.type = Value

  type TypedValue[+Caps[_]] = Value[Caps, Any, UType]
  val TypedValue: Value.type = Value

  type UDefinition = Definition[AnyType, Any, Any]
  val UDefinition: Definition.type = Definition

  type UPattern = Pattern[AnyType, Any]
  val UPattern: Pattern.type = Pattern

  type USpecification = Specification[Any]
  val USpecification: Specification.type = Specification

}
