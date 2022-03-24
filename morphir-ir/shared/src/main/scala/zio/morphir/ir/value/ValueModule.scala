package zio.morphir.ir.value

import zio.morphir.ir.{FQName, Name}

trait ValueModule {

  final type RawValue = zio.morphir.ir.value.RawValue
  val RawValue: zio.morphir.ir.value.RawValue.type = zio.morphir.ir.value.RawValue

  final type TypedValue = zio.morphir.ir.value.TypedValue
  val TypedValue: zio.morphir.ir.value.TypedValue.type = zio.morphir.ir.value.TypedValue

  def toRawValue[TA, VA](value: Value[TA, VA]): RawValue =
    value.mapAttributes(_ => (), _ => ())

  final def collectVariables[TA, VA](value: Value[TA, VA]): Set[Name] = value.collectVariables

  final def collectReferences[TA, VA](value: Value[TA, VA]): Set[FQName] = value.collectReferences

  def definitionToSpecification[TA, VA](definition: Definition[TA, VA]): Specification[TA] =
    definition.toSpecification

  def definitionToValue[TA, VA](definition: Definition[TA, VA]): Value[TA, VA] =
    definition.toValue

  def valuesAttribute[TA, VA](value: Value[TA, VA]): VA = value.attributes
}

object ValueModule extends ValueModule
