package zio.morphir.ir.value
import zio.prelude.AnyType
import zio.morphir.ir.ZEnvironmentSubset
import zio.morphir.ir.{FQName, Name}

trait ValueModule extends ValueSyntax {

  final type RawValue = zio.morphir.ir.value.RawValue
  val RawValue: zio.morphir.ir.value.RawValue.type = zio.morphir.ir.value.RawValue

  final type TypedValue[+Caps[_]] = zio.morphir.ir.value.TypedValue[Caps]
  val TypedValue: zio.morphir.ir.value.TypedValue.type = zio.morphir.ir.value.TypedValue

  final type Value[+Caps[_], +TA, +VA] = zio.morphir.ir.value.Value[Caps, TA, VA]
  val Value: zio.morphir.ir.value.Value.type = zio.morphir.ir.value.Value

  def toRawValue[Caps[_], TA, VA](value: Value[Caps, TA, VA]): RawValue = value.toRawValue

  final def collectVariables[Caps[_], TA, VA](value: Value[Caps, TA, VA]): Set[Name] = value.collectVariables

  final def collectReferences[Caps[_], TA, VA](value: Value[Caps, TA, VA]): Set[FQName] = value.collectReferences

  def definitionToSpecification[Caps[_], TA, VA](definition: Definition[Caps, TA, VA]): Specification[TA] =
    definition.toSpecification

  def definitionToValue[Caps[_], TA, VA](definition: Definition[Caps, TA, VA]): Value[Caps, TA, VA] =
    definition.toValue

  def valuesAttribute[Caps[_], TA, VA](value: Value[Caps, TA, VA]): ZEnvironmentSubset[Caps, VA] = value.attributes
}

object ValueModule extends ValueModule
